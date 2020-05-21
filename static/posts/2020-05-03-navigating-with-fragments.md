---
title: "Navigating with fragments"
description: "Navigating through multiple fragments in an Android application"
keywords: Android,Fragment,navigation,Bundle,FragmentTransaction,back stack
---

Navigating between different "places" of your app, is probably one of the most
complicated things in Android. There are many APIs that you can use depending
on what components you are using (activities or fragments), and how do you
want to pass data (if any?) between them. Unfortunately, I don't think there's 
a silver bullet for this, so I'll just write about the method that I like to use
the most.

<!--more-->

First of all, I think navigation is easier if you have a single activity with 
multiple fragments. In fact, [Google recommends using single-Activity apps](
https://www.reddit.com/r/androiddev/comments/8i73ic/its_official_google_officially_recommends_single/?user_id=172167102077). 
I know that there have been plenty of issues with fragments over the years, but 
these days with AndroidX libraries, they work great. Hopefully [they will get 
only better](https://www.youtube.com/watch?v=RS1IACnZLy4). This doesn't mean 
that you are restricted to having only one activity. There are valid cases for 
launching a new activity, but it's better to minimize those. 

The only problem that I still have with fragments is the back stack. It's nice 
to have an easy way to return to previous fragments, but the fragment back stack 
API is very limited and clumsy. Sure, you can go back to the last fragment with 
`popBackStack()`, but if you want to go back to an arbitrary earlier fragment,
or clear the whole stack, it's not very helpful. This is why I prefer to 
implement this stack myself and have full control over it. Going back to any
previous fragment is easier if the current fragment holds the data necessary to 
recreate any of those fragments.

A good place to store this data is the fragment's arguments. When you create a 
fragment, you can use `setArguments()` to pass data to it inside a `Bundle`. 
This is important because the OS will persist this data so it doesn't get lost 
when the fragment gets destroyed for whatever reason. This isn't  unique to 
fragments. It also happens to activities and extra data passed to them via 
`Intent`. If you pass your back stack data via arguments, it will never get 
lost.

Consider a scenario in which there is a `MyListFragment` and when users 
click one of the items in the list they navigate to `MyDetailFragment` so 
that they can see more data about the selected item and maybe do some 
editing. If they do edit it, then when they go back to the first fragment,
the update should be instantly reflected on the list.

Here's how I would implement this. The code for going into `MyDetailFragment` 
would be something like this:

``` Kotlin
val listState = captureListState()
val bundle = Bundle()
bundle.putParcelable("myListState", captureListState())

val newFragment = MyDetailFragment()
newFragment.arguments = bundle

fragmentManager.beginTransaction()
    .setCustomAnimations(
        R.anim.enter_from_right,
        R.anim.exit_to_left,
        R.anim.enter_from_left,
        R.anim.exit_to_right)
    .replace(R.id.container, newFragment)
    .commit()
```

The function `captureListState()` returns a `Parcelable` object that 
`MyDetailFragment` can retrieve from its arguments and use to reconstruct the 
list. Implementing `Parcelable` manually is error prone so I prefer to have 
some tool do it for me. I usually go with [data classes annotated with 
`@Parcelize`](https://android.jlelse.eu/yet-another-awesome-kotlin-feature-parcelize-5439718ba220), 
or [AutoValue](https://github.com/rharter/auto-value-parcel) if I have to do this 
in Java.

When `MyDetailFragment` is created, it should retrieve `listState` from 
arguments and keep it somewhere convenient like a view model so that it can
be updated if the user edits the data. Then, an `OnBackPresedCallback` should
be added to reconstruct `MyListFragment` when the user presses back.

``` Kotlin
private val onBackPressedCallback = object: OnBackPressedCallback(true) {
    override fun handleOnBackPressed() {
        val bundle = Bundle()
        bundle.putParcelable("savedListState", getUpdatedListState())

        val newFragment = MyListFragment()
        newFragment.arguments = bundle

        fragmentManager.beginTransaction()
            .setCustomAnimations(
                R.anim.enter_from_left,
                R.anim.exit_to_right,
                R.anim.enter_from_right,
                R.anim.exit_to_left)
            .replace(R.id.container, newFragment)
            .commit()
    }
}
```

Once again I replace the current fragment. There are no fragments to "pop" 
here. When the user presses back, a new instance of `MyListFragment` is 
created with the updated list state. Since there's no back stack collecting 
previous fragments, I am free to choose where to navigate, not just back to 
`MyListFragment`. Under certain conditions I may want to go back to somewhere
else like `HomeFragment` and completely forget about `MyListFragment`. With 
this approach, is super easy to choose a new destination in 
`handleOnBackPressed()`.

Notice that the animations used here are the "opposite" of the previous 
transaction. This gives the illusion of going back to the original fragment. 
End users will never realize that it's actually a new fragment with the same 
state. Rather than modifying existing fragments in a back stack, I create a
new "copy" of that fragment with updated data. I personally like this 
*functional* approach to managing fragments, but it is not perfect. 
Here are some potential issues:

### Scaling to larger stacks

This example works really nice because there's only one fragment the user can go
back to. In fact you can get the same result for free by calling 
`addToBackStack()` in the transaction.  But what if there were four, five, or 
an unbounded number? You'd have to add a value to the bundle for each of them. 
This isn't a common thing, most apps have only so many fragments in the back 
stack. But, if you are struggling with this, then it's better to use something 
like a queue in the arguments bundle that you pop each time user presses back. 
It might be a little more work, but it pays off. If you implement the most 
appropriate data structure for your use case, managing your fragment history is 
a breeze.

### Coupling fragments

For a fragment to recreate its previous fragment, it has to know about it. This
means that these two are **coupled**. Coupling is generally a bad thing in 
programming, and  this is no exception. Fragments are meant to be reusable. If
it only works if it follows a particular fragment, then it's not very reusable.

For example, if I have a `ChangePasswordFragment` I will certainly navigate to 
it from `SettingsFragment` so that users can change passwords at any time. 
Additionally, I may want to navigate to it from `LoginFragment` after an user 
clicks "Forgot my password" and proves to be the legitimate account owner. 
This fragment must be reused in both cases, but they must return to completely 
different places once the task is complete.

I shouldn't have to modify `ChangePasswordFragment` every time I want to reuse 
it. It's better if the fragment itself knows nothing about the fragment(s) that 
came before it. A solution for this would be to declare an abstract method in 
`ChangePasswordFragment` that gets called when the user should navigate away 
from it. It would be something like this:

``` Kotlin
abstract class PasswordChangeFragment: Fragment() {

    abstract fun onQuit()

    // actual change password UI & logic here. 
    // When the user presses back or sets a new password, 
    // onQuit() gets called.
} 
```

Then, `PasswordChangeSettingsFragment` and `PasswordChangeLoginFragment` extend 
it, recreating a different fragment in the `onQuit()` method. I'm personally not
a big fan of inheritance, but this seems like an appropriate case to use it.

### Complex views

For some views, it may be too difficult to capture its current state and 
recreate it in a new fragment. This may be even impossible for some custom 
views that only expose a limited API. Ideally all views should be simple and 
easy to reproduce, but sometimes it can't be helped.

I once tried to restore a `SearchView` inside a `Toolbar` after displaying 
search results in a new fragment. In the end, I  decided that it was far easier 
to just leave it alone and display search results in a secondary activity.

### Large amounts of data

Passing data in arguments is really convenient for persistence, but it is not 
meant to handle large amounts of data. Bear in mind that space for persisted 
data is limited and serialization should run as fast as possible. Arguments 
should only hold the minimum amount of data for the fragment to be able to work.

If a list fragment displays hundreds of thousands of items, it's probably 
not a good idea to put them all in a `Bundle`. If the data is loaded from a 
server, the it might be better to just reload it when the user returns to 
ensure they always see the most up-to-date data. If you are confident about the 
data not changing too often then you should cache it so that reloading is 
instant.

### Animations

There are a few very solid APIs for animating fragment transitions, but you may 
face issues. A quick Stack Overflow search shows hundreds of questions for 
fragment animations. The new [`FragmentContainerView` addresses one of these bugs](
https://proandroiddev.com/android-fragments-fragmentcontainerview-292f393f9ccf),
so make sure you are using it.

Even after fixing all bugs, not every animation may be possible with fragments. 
For instance, the only Android device I currently own runs Android Pie. I really 
like the default animation used for activities in this version. Unfortunately, 
that animation can't be recreated with fragments by simply copying the source, 
as it uses a [`cliprect` tag, which is a private component](
https://stackoverflow.com/questions/54221728/why-i-cant-use-cliprect-in-android-anim-resource-just-like-activity-open-ente). 
I have no idea why they do this.

This means that if I want to use that particular transition, I have to launch 
a new activity. There's no other way around it. And it goes without saying that 
newer versions of Android may replace it with something completely 
different, so its not reliable.

---

There may be other shortcomings that I've missed here, but I've had great 
results with this method. I use it 90% of the time. In very few cases, I
find it easier to just launch a new activity with `startActivityForResult()`. 
I think it's super important to use the best tool for the job instead of 
rooting for a one size fits all solution. 

I'm also keeping an eye on new APIs like the [Navigation component](
https://developer.android.com/guide/navigation). I'll be happy to adopt them 
once they become more stable, and prove to be objectively better, but for now 
I'm good with this.
