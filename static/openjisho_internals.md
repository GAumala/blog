
Basically, 
every screen  has its own `Fragment` class, which internally delegates another 
class to listen state changes and update the UI accordingly. State changes are
triggered by `Action` classes, which can produce side effects that can perform
indeterminate computations generating more `Action` instances later. To better 
illustrate this, here's the package directory for the "entry details" screen:

```
actions/
  PostKanji.kt
EntryFragment.kt
EntrySideEffect.kt
EntrySideEffectRunner.kt
EntryState.kt
EntryUI.kt
EntryViewModel.kt
```

`EntryFragment` inflates the view and instantiates other necessary classes. 
`EntryState` is a data class that models the screen's state. `EntryUI` holds 
references to all views and gets called every time the state changes to update 
the UI. `EntrySideEffect` has a bunch of data classes describing side effects. 
These get passed `EntrySideEffectRunner` as parameters to perform indeterminate 
computations. For this screen there's only one side effect that loads kanji 
information asynchronously. When that completes, it submits the `PostKanji` 
action to update the state and display the loaded information. This pattern
is used across all screens, so if you understand how this one works, you can 
understand all of them.

The side effect runner class is where stuff like database queries are executed.
This class's single responsibility is to start some work and submit an `Action` 
when it completes. So, it always uses dependency injection to obtain instances of 
the classes that do the actual work. I don't use any dependency injection 
frameworks. I think they are way too complex, specially for an app as small as 
this one. Any dependencies are instantiated and injected manually in the View 
model factory class like this:

``` kotlin
override fun <T : ViewModel?> create(modelClass: Class<T>): T {
    val ctx = f.activity as Context

    val viewModel = EntryViewModel()

    val initialState = createInitialState()
    val appDB = DictDatabase.getInstance(ctx)
    val runner = EntrySideEffectRunner(
        viewModel.viewModelScope, appDB.dictQueryDao())

    // more view model setup here 

    return viewModel
}
```

# First time setup

The first time the user opens the app, it has to download a bunch of dictionary 
files and store them in a database. This is necessary for dictionary queries to 
work offline. This can take a few minutes depending on your internet connection. 
It's important to keep the user posted about the setup's progress, otherwise 
users may think that the app froze or something. The setup can take way too much 
time for many phones, so the OS may kill the process.

For these reasons, the setup is executed with a foreground service, the 
`SetupService` class. This makes the OS less likely to kill the app when it needs
to reclaim some memory, but it's not perfect. Foreground services can display an 
ongoing notification, so I use that to notify the user about the setup's 
progress. Additionally, the service binds to `SetupFragment` so that I can 
display the setup progress here as well. 

`SetupService` only handles the notification and the binding. The actual setup 
work is delegated to the `SetupWorker` class. This makes it easy to write unit 
tests for the setup. This class starts a new coroutine for every file it needs to 
download. This way I can download, parse and persist in database all files 
concurrently. There's also a `ProgressController` class that uses a channel 
internally so that every coroutine can send a message periodically to report 
it's progress. This is how the setup starts: 

``` kotlin
suspend fun doWork(scope: CoroutineScope): Either<Exception, Unit> {
    // Initialize with 5 because there are going to be 6
    // coroutines reporting progress simultaneously, doing
    // the following tasks:
    // 1. Populate RADKFILE table
    // 2. Populate JMdict table
    // 3. Populate KANJIDIC table
    // 4. Download Tatoeba links file
    // 5. Download Tatoeba translations file
    // 6. Populate Tatoeba table
    val progressController = ProgressController(scope.assertStillActive, 6)
    progressController.manageReporters(scope, reportProgressToService)

    val radkfileReporter = progressController.getReporterWithPriority(0)
    val kanjidicReporter = progressController.getReporterWithPriority(1)
    val jmDictReporter = progressController.getReporterWithPriority(2)
    val tatoebaIndicesReporter = progressController.getReporterWithPriority(3)
    val tatoebaTranslationsReporter = progressController.getReporterWithPriority(4)
    val tatoebaReporter = progressController.getReporterWithPriority(5)

    // continue setup...
}
```

Only one task should show it's progress to the user at a time, 
otherwise it would be to confusing. Every task gets a `ProgressReporter` 
instance with a different priority. The priorities are hardcoded because it's 
easy to tell in which order the tasks will complete because their duration is 
proportional to the volume of the data they process, which is practically 
fixed. `ProgressController` starts by showing the progress of the reporter with 
priority 0. Only when that task completes, the next task in line can start 
reporting. 

Every task runs a function with return type `Either<Exception, Unit>`, which means
it can fail with an exception or succeed with `Unit` (No data). The function for 
each task is in the following classes:

- `JMdictDBSetup` inserts JMdict entries (dictionary data) to database.
- `RadkfileDBSetup` inserts RADKFILE (Kanji radicals data) entries to database.
- `KanjidicDBSetup` inserts KANJIDIC (Kanji data) entries to database.
- `TatoebaDBSetup` inserts Tatoeba example sentences with translations to database.

Every task is  represented by a `Deferred` object, so I put all of them in a queue and 
`await()` for them to complete in order.

```
private suspend fun processDeferredResultQueue(
    queue: Queue<Deferred<Either<Exception, Unit>>>
): Either<Exception, Unit> {

    var errorResult: Either.Left<Exception, Unit>? = null
    while (queue.isNotEmpty()) {
        val pendingTask = queue.remove()
        val result = pendingTask.await()

        if (result is Either.Left) {
            // result.value.printStackTrace()
            errorResult = result
        }

        if (queue.isEmpty())
            return errorResult ?: result
    }

    throw IllegalArgumentException("No tasks found in queue")
}
```

If any task fails, I immediately exit and report an error message. The user
then can click "retry" to restart the setup. It would be a huge waste of time
to restart from scratch so tasks also use checkpoints to avoid doing work 
that's already completed. For example `TatoebaDBSetup` checks if example sentences 
have already been inserted before attempting to even read the sentences file. 

``` kotlin
private fun populateDBWithJpnSentences(
    sentencesFile: File
): HashSet<Long> {
    if (checkpointManager.reachedCheckpoint(Checkpoint.sentencesReady))
        return dao.getJapaneseSentenceIds().toHashSet()

    val totalSentences = sentencesFile.countLines()
    val japaneseSentenceIds = HashSet<Long>()
    dao.runInTransaction(Runnable {
        // insert sentences...
    })
    checkpointManager.markCheckpoint(
        Checkpoint.sentencesReady, true
    )
    return japaneseSentenceIds
  }
```

If the sentences have already been inserted, it can skip straight to inserting 
translations. This way, the app can safely retry the setup as many times as 
necessary.

Another important thing when handling failiures is to resume interrupted 
downloads. For example sentences, the app has to download big files from Tatoeba's 
HTTP server. Resuming is as simple as adding a `Range` header to the HTTP request 
header.

``` kotlin
private fun openConnection(outputFile: File): HttpURLConnection {
    val urlObj = URL(url)
    val connection = urlObj.openConnection() as HttpURLConnection

    if (!outputFile.exists())
        return connection

    val downloadedSize = outputFile.length()
    connection.addRequestProperty("Range", "bytes=$downloadedSize-")

    // check response

    return connection
}
```

This way, if the download gets interrupted because some other task failed, we 
can ask the server to send bytes from the point we left off last time.

# Database

All data is stored in SQLite database using Room. Here is the data class used
for [JMdict entries](https://users.monash.edu/~jwb/jmdictart.pdf) :

``` kotlin
@Entity(tableName = "jmdict")
data class JMdictRow(
    @PrimaryKey val id: Long,
    val entryJson: String)
```

I store all the data as a single JSON string column because I'm not indexing 
any of it (at the moment), so I don't have to deal with migrations whenever 
I want to store more fields. After the data is queried, it is parsed and converted
to this class:

``` kotlin
@Parcelize
data class JMdictEntry(val entryId: Long,
                       val kanjiElements: List<Element>,
                       val readingElements: List<Element>,
                       val senseElements: List<Sense>): Parcelable {

    @Parcelize
    data class Element(val text: String, val tags: List<Tag>): Parcelable

    @Parcelize
    data class Sense(val glossItems: List<String>,
                     val glossTags: List<Tag>): Parcelable


    abstract class Tag: Parcelable {
        abstract fun getText(ctx: Context): String
        abstract fun getRawTag(): String
    }
```

All JSON serializing/deserializing is done manually with the built-in 
`JSONObject` class. I know there are tools like Gson that can do this without
having to write extra code, but I think that in the long run, it's simpler 
to do it manually.

Of course, running queries with a single JSON column would be a nightmare. 
Users expect to type in keywords and get a list of matching entries. With
this in mind, I collect all possible keywords (kanji, reading, and sense 
elements) in every entry and store them with the `JpnKeywordRow` entry 
referencing the entry id.

``` kotlin
@Entity(tableName = "jpn_keywords",
    foreignKeys = [
        ForeignKey(
            entity = JMdictRow::class,
            parentColumns = ["id"],
            childColumns = ["entryId"],
            onDelete = CASCADE
        )
    ],
    indices = [Index("keyword"), Index("entryId")]
)
data class JpnKeywordRow(
    @PrimaryKey(autoGenerate = true)
    val id: Long,
    val keyword: String,
    val entryId: Long)
 ```

This way, I can search on the keywords table, and then join with the JMdict 
entries table to fetch all the relevant data. Here's a query that does that:

``` kotlin
@Query("""
        SELECT $allJMdictRows FROM jmdict
        JOIN jpn_keywords 
        ON jmdict.id = jpn_keywords.entryId
        WHERE :queryText = keyword
    """)
    abstract fun lookupJMdictRowsExact(
        queryText: String
    ): List<JMdictRow>
 ``` 

This is pretty fast, but it only finds exact matches. Users might also want to
search entries "starting with this" or "ending with that". To do that, I use the 
`LIKE` operator. 

``` kotlin
@Query("""
        SELECT DISTINCT $allJMdictRows FROM jmdict
        JOIN jpn_keywords 
        ON jmdict.id = jpn_keywords.entryId
        WHERE keyword 
        LIKE :queryText 
        LIMIT :limit 
        OFFSET :offset
    """)
    abstract fun lookupJMdictRowsLike(
        queryText: String, 
        limit: Int, 
        offset: Int
    ): List<JMdictRow>
 ```

This works well, but it's a little slow. Fortunately, Room supports [full-text 
search](
https://developer.android.com/training/data-storage/room/defining-data#fts), so
I can do this:

```
@Query("""
        SELECT $allJMdictRows FROM jmdict
        JOIN eng_keywords 
        ON jmdict.id = eng_keywords.entryId
        WHERE keywords 
        MATCH :queryText 
        LIMIT :limit 
        OFFSET :offset
    """)
    abstract fun lookupJMdictRowsEnglishMatch(
        queryText: String, 
        limit: Int, 
        offset: Int
    ): List<JMdictRow>
```

This is super fast. full-text search is specially useful for searching
Tatoeba sentences. I'm not sure how accurately it handles languages other
than English, so I fall back to the `LIKE` operator for queries in Japanese.
I even have two different keyword tables for English and Japanese. This is
because the entity has to declare that it uses Full-Text Search. Here is the
English keyword entity:

``` kotlin 
@Entity(tableName = "eng_keywords")
@Fts4
data class EngKeywordRow(
    @PrimaryKey(autoGenerate = true)
    val rowid: Long,
    val entryId: Long,
    val keywords: String)
```

I'm happy with Room's support for FTS, but I wish there was an easy way to
rank results by relevance. *If only the mega corporation behind Room had some
experience with search engines...* 
