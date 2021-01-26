---
title: "Working with byte streams in Kotlin"
description: "Working with InputStream and OutputStream in Kotlin"
keywords: Android,Java,Kotlin,stream,InputStream,OutputStream
---

Sometimes it is necessary to process data that is so large that it is no 
longer practical to load it all into memory. This is often the case in mobile
apps, where computational resources are very limited, so it's better to stream 
the data and avoid an `OutOfMemoryException`. Recently I was working in an app 
written in Kotlin that processes big files, up to 300 MB, so I decided to write 
down some useful tips for working with byte streams in Kotlin.

<!--more-->

### Extension functions

The Kotlin standard library has lots of extension functions that improve 
existing Java APIs. Most of them are really easy to implement, but 
nevertheless it's great to have them at your disposal, plus, they often
make code more readable. Here are some of my favorite extensions for byte 
streams:

- [`File.inputStream()`](
  https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/input-stream.html) 
  Convenient function for opening an `InputStream` to a file.
- [`File.outputStream()`](
  https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/output-stream.html) 
  Convenient function for opening an `OutputStream` to a file.
- [`InputStream.reader()`](
  https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-input-stream/reader.html) 
  Creates a `Reader` from an `InputStream`. Useful for parsing text files. 
  `File` also has a similar function, in case you don't need the `InputStream`.
- [`Closeable.use()`](
  https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/use.html). This 
  makes sure that the stream is closed after you are done using it. This is 
  great for avoiding leaking resources.
- [`InputStream.copyTo()`](
  https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-input-stream/copy-to.html).
  This copies the contents of an `InputStream` to a `OutputStream`. This is
  super useful for copying or downloading files.
  
As an example of the usage of some of these functions here's how you could 
implement a function for copying files:

``` Kotlin
fun copyFile(sourceFile: File, destinationFile: File) {
    sourceFile.inputStream().use { input ->
        destinationFile.outputStream().use { output ->
          input.copyTo(output) 
        }
    }
}
```

This looks nice, but there's no need to add this to your codebase because there
is already and extension function for copying files, `File.copyTo()`, and it 
does exactly this but contains additional validations and configurable options.

The standard library is great, but it can only contain so many functions. As it
is included as a regular dependency in Android projects, new problems could arise
if it becomes too bloated. There are some things that you will have to 
implement yourself or look elsewhere. The next section is an example of this.

### Tracking the number of processed bytes

Neither Java nor Kotlin have a built-in solution for tracking the number of 
processed bytes in a stream so that the UI can display the progress of the task. 
This is important because if you are processing large amounts of data, it will
take a proportionally long amount of time. The user should be able to see the
progress of the task and estimate how much time is left to complete it.

The solution is to create wrapper classes that monitor the invocations of the 
`read()` and `write()` methods, tracking the number of processed bytes
and calling a lambda function whenever this number increases. Here's how I 
implemented it:

``` Kotlin
class ObservableInputStream(private val wrapped: InputStream,
                            private val onBytesRead: (Long) -> Unit): InputStream() {
    private var bytesRead: Long = 0

    @Throws(IOException::class)
    override fun read(): Int {
        val res = wrapped.read()
        if (res > -1) {
            bytesRead++
        }
        onBytesRead(bytesRead)
        return res
    }

    @Throws(IOException::class)
    override fun read(b: ByteArray): Int {
        val res = wrapped.read(b)
        if (res > -1) {
            bytesRead += res
            onBytesRead(bytesRead)
        }
        return res
    }

    @Throws(IOException::class)
    override fun read(b: ByteArray, off: Int, len: Int): Int {
        val res = wrapped.read(b, off, len)
        if (res > -1) {
            bytesRead += res
            onBytesRead(bytesRead)
        }
        return res
    }

    @Throws(IOException::class)
    override fun skip(n: Long): Long {
        val res = wrapped.skip(n)
        if (res > -1) {
            bytesRead += res
            onBytesRead(bytesRead)
        }
        return res
    }

    @Throws(IOException::class)
    override fun available(): Int {
        return wrapped.available()
    }

    override fun markSupported(): Boolean {
        return wrapped.markSupported()
    }

    override fun mark(readlimit: Int) {
        wrapped.mark(readlimit)
    }

    @Throws(IOException::class)
    override fun reset() {
        wrapped.reset()
    }

    @Throws(IOException::class)
    override fun close() {
        wrapped.close()
    }
}

class ObservableOutputStream(private val wrapped: OutputStream,
                           private val onBytesWritten: (Long) -> Unit): OutputStream() {
    private var bytesWritten: Long = 0

    @Throws(IOException::class)
    override fun write(b: Int) {
        wrapped.write(b)
        bytesWritten++
        onBytesWritten(bytesWritten)
    }

    @Throws(IOException::class)
    override fun write(b: ByteArray) {
        wrapped.write(b)
        bytesWritten += b.size.toLong()
        onBytesWritten(bytesWritten)
    }

    @Throws(IOException::class)
    override fun write(b: ByteArray, off: Int, len: Int) {
        wrapped.write(b, off, len)
        bytesWritten += len.toLong()
        onBytesWritten(bytesWritten)
    }

    @Throws(IOException::class)
    override fun flush() {
        wrapped.flush()
    }

    @Throws(IOException::class)
    override fun close() {
        wrapped.close()
    }
}
```

**EDIT 2021-01-14:** A [minor bug](https://github.com/GAumala/blog/issues/12) 
in `ObservableInputStream` has been fixed.

**EDIT 2021-01-25:** [It has been brought to my attention](
https://github.com/GAumala/blog/issues/13) that not all `InputStream` methods 
were originally implemented in this snippet and this could cause problems. 
Methods `available()`, `mark()`, `markSupported()`, `skip()`, and `reset()` 
have now been added. Also, Java 9 introduces new  `InputStream` methods 
`readNBytes()` and `readAllBytes()`. If you target Java 9 or later then you 
should override these methods as well to update the read bytes counter 
appropriately, otherwise you may end up with mysterious bugs. 

To use this, wrap the original stream with one of these classes, attaching a 
lambda function to execute every time the number of processed bytes increases.
For example here's how you could report progress while downloading a file via 
HTTP:

``` Kotlin
fun downloadFileViaHTTP(url: String, destinationFile: file) {
    val urlObj = URL(url)
    val connection = urlObj.openConnection()

    // To calculate progress, we need to now the total size 
    // beforehand. We can get that info from HTTP headers
    val contentLength = connection.getHeaderField("Content-Length")
    val totalDownloadSize = contentLength.toLong()

    // Wrap the connection input stream with ObservableInputStream
    // in order to monitor it.
    val urlInputStream = ObservableInputStream(connection.getInputStream()) {
        val progress = it * 100 / totalDownloadSize
        // updateProgress() should post a message to the UI thread 
        // to update a progress bar or a similar widget
        updateProgress(progress.toInt())
    }

    urlInputStream.use { input ->
        destinationFile.outputStream().use { output ->
            input.copyTo(output)
        }
    }
}
```

Once again I use the convenient `InputStream.copyTo()` function to move bytes
from the `ObservableInputStream` into the `OutputStream` that writes the file
to internal storage. But, what if you wanted to do the opposite? There is no 
`OutputStream.copyTo()`, so how could you transfer data from `OutputStream` to
`InputStream`?

### Transferring data from `OutputStream` to `InputStream`

This might sound a little odd, but there are legitimate cases for doing this. 
Just google "Java OutputStream to InputStream" and you'll get thousands of 
results. Besides, it's a good use case for one of Kotlin's most recent features. 
Ideally you should always read from `InputStream` and then write into 
`OutputStream`, but imagine a scenario in which you know that some external 
library is about to write data to an `OutputStream` but you'd like to read 
that output, apply some transformation and then write it to a new stream. 

At first glance this seems impossible because `OutputStream` doesn't have a 
`read()` method, only `write()`. Java's byte streams aren't as easy to compose
as UNIX streams in which you can just use the `|` operator to redirect the 
output of one program to the input of another. What you would have to do here is 
write to a "mocked" `OutputStream` that doesn't really write data anywhere, 
just holds on to it until you request it. There is a Java class for this, 
[`PipedOutputStream`](
https://docs.oracle.com/javase/7/docs/api/java/io/PipedOutputStream.html), but
there's still one little problem.

`PipedOutputStream` lets you create a `PipedInputStream` so that you can 
`read()` the data that is being written, but none of this changes the fact that 
`write()` blocks the calling thread.  This means that if you want to read the 
data as it is being written, you need to do it **concurrently**. The good news 
is that Kotlin has [coroutines](
https://kotlinlang.org/docs/reference/coroutines-overview.html) now,
so you can easily offload the writing to new coroutine, and read on the current 
one.

Finally, here is a little example to illustrate how to use `PipedOutputStream`. 
Suppose you want to download a file via FTP, but you want to decompress it 
before it gets written to the internal storage. The solution is to download 
in one coroutine and decompress in the other one. 

``` Kotlin
private fun downloadStream(ftpClient: FTPClient,
                           fileName: String,
                           output: PipedOutputStream) {
    // ftpClient immediately writes the downloaded bytes
    // to the output stream
    ftpClient.retrieveFile(fileName, output)
}

private fun decompressAndWriteStream(
    sourceStream: PipedOutputStream
    destinationStream: OutputStream) {

    val srcInputStream = PipedInputStream(sourceStream)
    val decompressedInputStream =
        GZIPInputStream(srcInputStream)
    decompressedInputStream.copyTo(destinationStream)
}

suspend fun downloadFile(coroutineScope: CoroutineScope,
                         destFile: File,
                         fileName: String) {
    val ftpClient = FTPClient()
    prepareForDownload(ftpClient)
    val fileOutputStream = destFile.outputStream()

    try {
        val pipedOutputStream = PipedOutputStream()

        // launch a new coroutine to download the data
        val job = coroutineScope.launch {
            downloadStream(ftpClient, fileName, pipedOutputStream)
        }

        // decompress in the current coroutine while 
        // the other one downloads the data
        decompressAndWriteStream(
            sourceStream = pipedOutputStream,
            destinationStream = fileOutputStream)

        // Once the file is completely written, the coroutine should 
        // have finished, but just to be safe make sure it exits.
        job.join()

    } finally {
        fileOutputStream.close()
        ftpClient.disconnect()
    }
}
```

Coroutines make dealing with concurrency much easier. In fact, I was very 
surprised to see this code work correctly on my first try. Since I process
multiple large files in parallel I make extensive use of Coroutines. I think
this is my favorite feature and the biggest reason to switch to Kotlin for 
Android development.
