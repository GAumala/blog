---
title: "Implementing MVI in Android"
description: "Writing maintainable android apps with the Model-View-Intent design pattern"
keywords: Android,Java,MVI,AutoValue,architecture,design pattern,library
---

Android development should be easy. Most apps that I've worked in aren't that 
complex, they usually boil down to just displaying some data from a remote 
server and then letting the user post new data to that server. Nevertheless, 
their codebases often ended up being ugly and convoluted. After all these years
I think I have finally figured out how to avoid common pitfalls.

<!--more-->

I think that the main reason why messy codebases are so common is that 
Android developers were left on their own when it came to app architecture. 
Google didn't care about it until [Google I/O 2017, when they 
announced architecture components](https://www.youtube.com/watch?v=FrteWKKVyzI). 
Even today, almost all Android guides from Google show examples that directly 
dump all code into activities of fragments, leading to chaos as the app grows.
I understand that they do this for the sake of brevity, but since developers 
(myself included) tend to copy & paste snippets from these guides, it ends up 
being problematic.

I was very excited after the architecture components announcement. I think 
it's really cool that finally Google decided to give developers tools 
to address the elephant in the room. In particular, I believe `ViewModel` and 
`LiveData` could be great building blocks for a solid architecture. I'm a big
fan of [The Elm Architecture](https://guide.elm-lang.org/architecture/), among
other functional languages, so I tried figure to how to use architecture 
components to bring some of these great ideas found in Elm and the likes into 
my Android apps. 

Among all the popular design patterns found in the Android community, 
[Model-View-Intent (MVI)](http://hannesdorfmann.com/android/model-view-intent) 
seemed like the closest one to what I wanted. I decided to implement it with 
architecture components in some of my apps and after some trial and error, 
extracted the useful classes into [this library](
https://github.com/GAumala/mvi-android) to make it easier to implement 
the pattern effectively in new apps. 

I am very pleased with the results so in this post I want to explain why MVI is
so effective, and how I use this library in my apps. For this matter I'll provide 
an example app, a tiny Reddit client in which you enter the name of a subreddit 
and load the current top posts. 

![](../assets/images/mvi_app_1.jpg)

![](../assets/images/mvi_app_2.jpg)

You can find the entire source code in the [library's repository](
https://github.com/GAumala/mvi-android). Feel free to checkout the code and 
run it in your own devices. In the following sections I'm going to break down 
the important parts and show how I use the library to implement the pattern. 

### The State

The most important thing in this application is to manage state and keep the UI 
synchronized with it. What would the state for this app look like? If I were to
draw it as [finite state machine](
https://en.wikipedia.org/wiki/Finite-state_machine) diagram, it would have the 
following nodes:

- **Input state**. Initially, there should be a form for the user to input the 
  name of a subreddit to load the posts.
- **Loading state**. After submitting a valid subreddit name, the app should
 show a spinner while loading the posts from the network.
- **Posts ready state**. If posts are loaded successfully, they should
 be displayed with the option to click and open the links in a browser.
- **Error state** If a network error occurs while loading posts, an error 
  message must be displayed.

Now let's define a class that models this state. State classes should just be 
immutable data. You should be able to derive `equals()`, `toString()`, and 
even implement `Parcelable` with little or no effort. If you are using Java I 
recommend using [AutoValue](
https://github.com/google/auto/blob/master/value/userguide/index.md) for this 
matter. If you are using Kotlin, [data classes](
https://kotlinlang.org/docs/reference/data-classes.html) are all you need.

Here's the state for our Reddit client:

``` Java
public abstract class RedditState {

    // Display a form so that the user can submit a 
    // subreddit name to load posts. 
    @AutoValue
    public static abstract class Input extends RedditState {
        public abstract @StringRes
        int errorResId();

        public static Input create(int errorResId) {
            return new AutoValue_RedditState_Input(errorResId);
        }
    }

    // Posts are loading, better show a spinner in the meantime.
    @AutoValue
    public static abstract class Loading extends RedditState {
        public abstract String subredditName();

        public static Loading create(String subredditName) {
            return new AutoValue_RedditState_Loading(subredditName);
        }
    }

    // Posts are ready to be displayed.
    @AutoValue
    public static abstract class Ready extends RedditState {
        public abstract String subredditName();

        public abstract List<Post> posts();

        public static Ready create(String subredditName, List<Post> posts) {
            return new AutoValue_RedditState_Ready(subredditName, posts);
        }
    }


    // Something went wrong loading the posts. 
    //Show an error message
    @AutoValue
    public static abstract class Error extends RedditState {
        public abstract String message();

        public static Error create(String message) {
            return new AutoValue_RedditState_Error(message);
        }
    }

    public static RedditState createInitialState() {
        return Input.create(-1);
    }
}
```

As you can see, every possible state is modeled as a `RedditState` subclass 
using `AutoValue`. It is similar to Kotlin's sealed classes. The disadvantage 
here is that Java lacks pattern matching for types, so it requires some unsafe 
casts. It's still good enough for me because it helps me avoid null references. 

### Side Effects

Now that the state is ready we have to define the side effects. Ideally all of
our code should have pure functions. These special functions are preferred 
because they always return the same values if provided with the same parameters. 
If all of our functions were so predictable, then fixing bugs is very easy and 
straight forward. Just run the function again with those same parameters on any 
environment and locate the line where it went wrong. Unfortunately, an app like 
that isn't very useful. Non determinism is unavoidable and even desired in a few 
parts of almost every app. A few examples of this are: 

- Random number generation
- Using a file system
- Sending & Receiving data over a network


In order to be able to trigger non-deterministic computations in pure functions, 
these functions will return side-effect values. These values will be passed to 
another object, a `SideEffectRunner`, which will read the data and figure out how
to execute the desired side-effect and post the result.

The only side effect that we want in this Reddit client, is the ability to fetch a
subreddit's top posts from Reddit's servers. This is non-deterministic because 
the top posts change over time, and the request can fail due to a network error.

Here's the definition of the `RedditSideEffect` class:

``` Java
public abstract class RedditSideEffect {
    @AutoValue
    public static abstract class FetchPosts extends RedditSideEffect {
        public abstract String subredditName();

        public static FetchPosts create(String subredditName) {
            return new AutoValue_RedditSideEffect_FetchPosts(subredditName);
        }
    }
}
```

The `FetchPosts` class has only one attribute: the subreddit name. This is 
because these classes are plain values with the minimum necessary data for the
`SideEffectRunner` to execute it. `FetchPosts` is a subclass of 
`RedditSideEffect` because different side-effects need different kinds of data.
In a full-featured Reddit client there should be more side-effects, like the 
ability to submit new posts, or upvote/downvote existing ones. Each of them 
would need different data, so they would be modeled with different subclasses 
of `RedditSideEffect`.

While `FetchPosts` contains the parameters to run the side effect. The class 
that actually executes it is `RedditSideEffectRunner`:

``` Java
import com.gaumala.mvi.ActionSink;
import com.gaumala.mvi.SideEffectRunner;

public class RedditSideEffectRunner
        implements SideEffectRunner<RedditState, RedditSideEffect> {
    private final Resources resources;

    public RedditSideEffectRunner(Resources resources) {
        this.resources = resources;
    }

    @Override
    public void runSideEffect(ActionSink<RedditState, RedditSideEffect> sink,
                              RedditSideEffect sideEffect) {

        if (sideEffect instanceof RedditSideEffect.FetchPosts)
            fetchPosts(sink, (RedditSideEffect.FetchPosts) sideEffect);

    }

    private void fetchPosts(ActionSink<RedditState, RedditSideEffect> sink,
                            RedditSideEffect.FetchPosts sideEffect) {

        FetchPostsTask.run(
              resources,
              sideEffect.subredditName(),
              res -> sink.submitAction(FetchPosts.create(res)));

    }
}
```

This class has a public method `runSideEffect()`, which takes an `ActionSink` 
object and an `RedditSideEffect` value. It identifies the 
`RedditSideEffect.FetchPosts` value and runs the desired effect using 
`FetchPostsTask`, a class executes the HTTP request in a background 
thread and invokes a callback lambda once the work is complete. 

The `ActionSink` object is used to update the state with the result of the
side effect. This interface exposes a single method: `submitAction()`. As
implied by the name it receives "actions", or intents to do something stateful,
 which I'll describe in the next section.

### Actions

Actions are values that can update the state. They have a `update()` method 
that takes the current state, and returns a new one, because the state is an 
immutable value. Optionally, this method can also return a side effect to be 
executed immediately. This method assumed to be a pure function, it can't 
perform I/O or mess with global variables to calculate the new state . It can 
only take into account the action's data and the current state. Here's the 
definition of the `FetchPosts` action mentioned on the previous section:

``` Java
import com.gaumala.mvi.Action;
import com.gaumala.mvi.Update;

@AutoValue
public abstract class FetchPosts 
  extends Action<RedditState, RedditSideEffect> {

    public abstract FetchPostsRes res();

    @NonNull
    @Override
    public Update<RedditState, RedditSideEffect> update(
        RedditState currentState) {

        if (!(currentState instanceof RedditState.Loading))
            return new Update<>(currentState);

        RedditState.Loading state = (RedditState.Loading) currentState;
        FetchPostsRes res = res();
        if (res instanceof FetchPostsRes.Success)
            return updateWithSuccess(state, (FetchPostsRes.Success) res);

        return updateWithError((FetchPostsRes.Error) res);
    }

    private Update<RedditState, RedditSideEffect> updateWithError(
            FetchPostsRes.Error res) {

        RedditState newState = RedditState.Error.create(res.message());
        return new Update<>(newState);
    }

    private Update<RedditState, RedditSideEffect> updateWithSuccess(
            RedditState.Loading state,
            FetchPostsRes.Success res) {

        RedditState newState = RedditState.Ready.create(
                state.subredditName(),
                res.posts());
        return new Update<>(newState);
    }

    public static FetchPosts create(FetchPostsRes res) {
        return new AutoValue_FetchPosts(res);
    }
}
```

This action takes as constructor parameter the result of `FetchPostsTask`: 
`FetchPostsRes`, which can either be `Success` or `Error`, and returns a new
state of type `Ready` or `Error` respectively. This action handles the server's
response, but what about the action that triggers the side effect that sends
the request? It is this one, `CallFetchPosts`:

``` Java
import com.gaumala.mvi.Action;
import com.gaumala.mvi.Update;

@AutoValue
public abstract class CallFetchPosts
        extends Action<RedditState, RedditSideEffect> {
    public abstract String subredditName();

    @NonNull
    @Override
    public Update<RedditState, RedditSideEffect> update(RedditState state) {

        if (!(state instanceof RedditState.Input))
            return new Update<>(state);

        if (subredditName().isEmpty()) {
            RedditState newState = RedditState.Input.create(
                    R.string.empty_string_error);
            return new Update<>(newState);
        }

        RedditState newState = RedditState.Loading.create(subredditName());
        RedditSideEffect sideEffect =
                RedditSideEffect.FetchPosts.create(subredditName());
        return new Update<>(newState, sideEffect);
    }

    public static CallFetchPosts create(String subredditName) {
        return new AutoValue_CallFetchPosts(subredditName);
    }
}
```

This action takes the subreddit name that the user entered, validates that it 
is not empty and then returns a new `Loading` state along with a side effect. 
This returned side effect is then passed to `RedditSideEffectRunner` so that 
it can execute `FetchPostsTask` and finally submit a `FetchPosts` action to
`ActionSink` with the result.

### Views

Every time an action updates the state the views should readjust themselves to 
let the user visualize the new state. To do this, the application's views should
be managed by a "UI" object that extends `BaseUI<T>`. This abstract class has a
`rebind()` method that has a single parameter: the current state. This method is
called after every `update()` so that the views always stay in sync with the 
state.

Before showing the implementation of `rebind()` for the Reddit client, let's talk
about the layout and what views are used. Here's the XML:

``` XML
<?xml version="1.0" encoding="utf-8"?>
<FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    xmlns:app="http://schemas.android.com/apk/res-auto">

    <androidx.appcompat.widget.Toolbar
        android:id="@+id/toolbar"
        android:layout_width="match_parent"
        android:layout_height="?attr/actionBarSize"
        android:elevation="4dp"
        app:title="@string/reddit"
        app:theme="@style/ThemeOverlay.AppCompat.Dark.ActionBar" />

    <include layout="@layout/subreddit_form_view"
        android:layout_marginTop="?attr/actionBarSize"
        android:layout_width="match_parent"
        android:layout_height="match_parent" />

    <androidx.recyclerview.widget.RecyclerView
        app:layoutManager="androidx.recyclerview.widget.LinearLayoutManager"
        android:id="@+id/posts_recycler"
        android:layout_marginTop="?attr/actionBarSize"
        android:visibility="gone"
        android:layout_width="match_parent"
        android:layout_height="match_parent"/>

</FrameLayout>
```

This layout has 3 main elements:

- Toolbar
- RecyclerView
- Input form (Nested layout with the form for the subreddit name).

The idea is to switch between the form and the `RecyclerView` depending on the 
state. If the state is instance of `Input`, the form should be visible while the
`RecyclerView` is hidden. For any other states, the `RecyclerView` is shown 
instead because progress bars, error messages and posts can be shown inside it. 
With that being said, here's a snippet of `RedditGUI` containing the `rebind()` 
implementation:

``` Java
import com.gaumala.mvi.ActionSink;
import com.gaumala.mvi.BaseUI;

class RedditGUI extends BaseUI<RedditState> {

    private final Context ctx;
    private final ActionSink<RedditState, RedditSideEffect> sink;
    private final ActionBar actionBar;

    private final View inputForm;
    private final TextInputLayout subredditInputLayout;
    private final View submitButton;
    private final GroupAdapter postsAdapter;
    private final RecyclerView postsRecycler;
    private final Toolbar toolbar;

    RedditGUI(@NonNull LifecycleOwner owner,
              @NonNull LiveData<RedditState> liveState,
              ActionSink<RedditState, RedditSideEffect> sink,
              View view,
              ActionBar actionBar) {
        super(owner, liveState);

        this.ctx = view.getContext();
        this.sink = sink;
        this.actionBar = actionBar;
        this.postsAdapter = new GroupAdapter();

        inputForm = view.findViewById(R.id.subreddit_input_form);
        subredditInputLayout = view.findViewById(R.id.subreddit_input_layout);
        submitButton = view.findViewById(R.id.submit_button);
        postsRecycler = view.findViewById(R.id.posts_recycler);
        toolbar = view.findViewById(R.id.toolbar);

        // set the adapter and dividers to RecyclerView
        setupRecyclers();
    }

    @Override
    public void rebind(RedditState state) {
        if (state instanceof RedditState.Input)
            showInputForm((RedditState.Input) state);
        else
            showPostsRecyler(state);

        // show the subreddit name in the title, or just "Reddit" 
        // if the user hasn't picked a subreddit yet
        toolbar.setTitle(getTitle(state));
    }
```

This class holds references to all the views and shows only the appropriate 
views for the current state every time `rebind()` is called. The `rebind()`
method makes no assumptions about the state transitions, it simply *reacts*
to the current state calling the necessary methods on every view. You may
be inclined to believe that resetting properties on every view after every
update might be inefficient, but it really isn't because android views are
smart enough to avoid redrawing things that haven't actually changed.  

You may notice that it calls a `super` constructor with two parameters of type 
`LifecycleOwner` and `LiveData<RedditState>` respectively. These two objects
are used under the hood to subscribe to changes in the state and call 
`rebind()` after every update.

Adjusting views isn't `RedditGUI`'s only responsibility. It also handles UI 
events like button clicking and window scrolling. Just like with 
`RedditSideEffectRunner`, `RedditGUI` also receives an `ActionSink` to submit 
actions and trigger state changes in response to UI events. For example, here's 
how the `showInputForm()` method used in `rebind()` sets a click listener 
to the submit button:

``` Java
private void showInputForm(RedditState.Input state) {

    // ...adjust some views

    // handle click event
    submitButton.setOnClickListener(v -> {
        String inputText = subredditInputLayout
                .getEditText().getText().toString();
        sink.submitAction(CallFetchPosts.create(inputText));
    });
}
```

When `submitButton` is clicked, a `CallFetchPosts` action is submitted with
the text input by the user.


### The Dispatcher

MVI establishes an unidirectional cycle between three parts: 
intent -> model -> view. In this app, these parts are represented as follows:

- **Intent**: Listening to UI events or side effect results is handled by the
  `ActionSink`. Both `RedditGUI` and `RedditSideEffectRunner` call this object
  when they have to deliver actions.
- **Model**: Processing data from intents to generate new states is handled by 
  `Action` classes and their `update()` method. 
- **View**: Readjusting the Views in order to reflect the latest state returned 
  by `update()` is handled by `RedditGUI` and its `rebind()` method. As this 
  class also generates intents, it is evident how things come full circle.

Some people view this pattern as the following function composition: 
`view(model(intent()))`. That composition is still present in this app, but it 
uses class methods instead of plain functions because we are still stuck in 
Java's OOP world. It's roughly something like this: 
`rebind(update(submitAction()))`. 

To glue everything together some sort of "observer" is needed. Here the 
`Dispatcher` class implements this functionality. As you may have already 
guessed it dispatches actions, triggering state changes and side effects. It 
implements `ActionSink`, the interface that `RedditGUI` and 
`RedditSideEffectRunner` use submit actions. It takes a `SideEffectRunner` 
object in its constructor so that it can execute the side effects returned by 
the received actions. Additionally, it holds the `LiveData` object with the 
current state so it manages the state and lets observers like `RedditGUI` react 
to state changes. 

Unlike `BaseUI` or `SideEffectRunner`, you don't have to extend this 
class, you simply create an instance with the appropriate type parameters. 

The dispatcher is kept inside a `ViewModel` so that the application state can 
persist configuration changes like screen rotation. There is a 
`DispatcherViewModel` class that does exactly that. It is parametrized just like
`Dispatcher`, but since you don't instantiate view models directly in android, 
it's better to extend `DispatcherViewModel` with the desired type parameters. 
For example, here's the view model used for the Reddit client:

``` Java
import com.gaumala.mvi.DispatcherViewModel;
import com.gaumala.mvi.Dispatcher;

public class RedditViewModel 
    extends DispatcherViewModel<RedditState, RedditSideEffect> {

    RedditViewModel(Dispatcher<RedditState, RedditSideEffect> dispatcher) {
        super(dispatcher);
    }
}
```

This class doesn't do anything special. It is merely a convenience needed due to
the parametrization of the `DispatcherViewModel` type. Since it extends 
`DispatcherViewModel`,  it gets the public methods: `getLiveState()`, and 
`getActionSink()` which return objects of type `LiveData<RedditState>` and 
`ActionSink<RedditState, RedditSideEffect>` respectively. These two objects are 
needed to connect all the classes implemented so far because they are able to 
read the current state and update it via actions.  

### Running the app

Now that there's a view model that persists the dispatcher and application state
it's time to wire up everything and run the app. The first thing to do is to 
instantiate the view model along with all the dependencies (the initial  state, 
dispatcher and side effects runner). I like do this in implementations of 
`ViewModelProvider.Factory`. 

``` Java
import com.gaumala.mvi.Dispatcher;

public class ViewModelFactory implements ViewModelProvider.Factory {
    private final Fragment fragment;

    public ViewModelFactory(Fragment fragment) {
        this.fragment = fragment;
    }

    @NonNull
    @Override
    public <T extends ViewModel> T create(@NonNull Class<T> modelClass) {
        // The fragment's arguments could be used here to create 
        // the initial state
        RedditState initialState = RedditState.createInitialState();

        RedditSideEffectRunner runner =
                new RedditSideEffectRunner(fragment.getResources());

        // optionally, startup side effects could be run here

        Dispatcher<RedditState, RedditSideEffect> dispatcher =
                new Dispatcher<>(runner, initialState);

        return (T) new RedditViewModel(dispatcher);
    }
}
```

The `create()` method is guaranteed by the architecture components library to 
run only once, when the view model has not been yet created. It is not necessary 
for `ViewModelFactory` to keep a reference to the fragment, but I do it because 
it is very often useful. In this case it gives me access to a `Resources` 
reference that I use in in `RedditSideEffectRunner` to create appropriate error 
messages. Also, The initial state could created with a function that returns a 
different value depending on the fragment's argument bundle.

This view model is instantiated by `RedditFragment`, the fragment that renders the
views for our Reddit client. Here's how it starts the app:

``` Java
public class RedditFragment extends Fragment {
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater,
                             @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        setHasOptionsMenu(true);
        RedditViewModel viewModel = ViewModelProviders
                .of(this, new ViewModelFactory(this))
                .get(RedditViewModel.class);

        View view = inflater.inflate(
                R.layout.reddit_fragment, container, false);
        ActionBar actionBar = setupActionBar(view.findViewById(R.id.toolbar));

        gui = new RedditGUI(
                this.getViewLifecycleOwner(),
                viewModel.getLiveState(),
                viewModel.getUserActionSink(),
                view, actionBar);
        gui.subscribe();

        return view;
    }
}
```

`RedditFragment` only implements one lifecycle method: `onCreateView()` which 
does the following two things before returning a view:

1. Creates the view model if it doesn't already exist. 
2. Creates a `RedditGUI` fetching a few dependencies from the view model so 
  that it can observe state changes and submit actions. 
3. Calls the `RedditGUI.subscribe()` method to start observing state changes.

That's it! This is all that's necessary to get the app running. Notice that 
there is no need to implement `onStop()` or any other lifecycle method because
`LiveData` object used by `RedditGUI` automatically unsubscribes from state
changes when the fragment stops. Network requests could still be loading while
the fragment is stopped or recreated and there won't be any memory leaks because 
the side effects runner never keeps a reference to the activity or fragment. 
More complex apps may need to implement other lifecycle methods, and that's ok, 
but most of the time it is not necessary thanks to `LiveData`. 

### Testing

I think testing is very important, and a good architecture should make it easy
to write tests. Since state and actions are defined as immutable values it is
very easy to come up with reproducible test cases. You can quickly verify if 
a chain of actions ends up with a particular state or if it returns any desired
side effects. 

One of the reasons why `ActionSink` is an interface instead of a concrete class
is that this makes it possible to use a different implementation in in testing
environments that doesn't rely on the android framework and exposes additional
methods for making assertions about the generated states and side effects.

Here's a test that asserts that the app reaches the `RedditState.Ready` state
when the user inputs a valid subreddit and the server request succeeds:

``` Java
@Test
public void should_display_posts_in_absence_of_errors() {
    RedditState initialState = RedditState.createInitialState();
    TestSink<RedditState, RedditSideEffect> sink =
            new TestSink<>(initialState);

    sink.submitAction(CallFetchPosts.create("news"));
    sink.submitAction(FetchPosts.create(
            ResponseMocks.fetchPosts_Success));

    RedditState currentState = sink.getCurrentState();
    RedditState expectedState = RedditState.Ready.create(
        "news",
        ResponseMocks.fetchPosts_Success.posts());
    assertThat(currentState, is(equalTo(expectedState)));
}
```

This test uses a custom `ActionSink` implementation, `TestSink` that
exposes a `getCurrentState()` method that lets you peek into the current state 
so that you can make assertion about its value. It also exposes a 
`getGeneratedSideEffects()` to peek into all the generated side effects so far.
Here's another test that asserts that a `FetchPosts` side effect is generated 
by these same two actions:

``` Java
@Test
public void should_generate_FetchPosts_side_effect_in_absence_of_errors() {
    RedditState initialState = RedditState.createInitialState();
    TestSink<RedditState, RedditSideEffect> sink =
            new TestSink<>(initialState);

    sink.submitAction(CallFetchPosts.create("news"));
    sink.submitAction(FetchPosts.create(
            ResponseMocks.fetchPosts_Success));


    List<RedditSideEffect> actualSideEffects = sink.getGeneratedSideEffects();
    List<RedditSideEffect> expectedSideEffects = Collections.singletonList(
            RedditSideEffect.FetchPosts.create("news"));
    assertThat(actualSideEffects, is(equalTo(expectedSideEffects)));
}
```

### "Real" apps

This tiny Reddit client only has one fragment because it only does one thing: 
fetch posts from a particular subreddit. This isn't the kind of app I'm 
targeting but it's good enough as an example. A more realistic Reddit client
would let you do a lot more things like:

- Sign in to your Reddit account
- Fetch posts from your frontpage
- Manage your subscriptions
- Check your DMs

The list goes on, but I'm sure you get the idea. "Real" apps have lots of 
features. With this design pattern, each of these features would be 
implemented in its own fragment, defining its own state and side effects, 
isolated from the other features. It's like having your app made of dozens 
of little apps that are easy to manage. 

The app I'm currently working on in my day job is made of 24 "little apps" 
occupying 3 MB just in Java source files. It also has an additional 3 MB of 
legacy java code that I can't really touch, let alone convert to this pattern. 
It's been less than 6 months of development and it is very likely that we'll
add many more features before the year ends. No matter how much it grows I'm
confident that it will remain as maintainable as always.
