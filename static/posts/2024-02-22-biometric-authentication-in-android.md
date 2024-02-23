---
title: "Biometric authentication on Android"
description: "Secure authentication on Android with the new Biometric library"
keywords: Android,Biometric,Cryptography,Fingerprint
---

I recently tried out Biometric authentication on Android using the new
[Biometric library](
https://developer.android.com/reference/androidx/biometric/package-summary).
The new API is really good. The OS now provides a dialog for biometric 
authentication, making things smoother and more consistent for users. Google even
has [this neat guide](
https://developer.android.com/training/sign-in/biometric-auth) to help get you
started with it. Unfortunately it is a bit lackluster, it was not clear
to me how to get the whole thing running correctly. I wanted to 
expand on it and write a better guide on how to store a key for 
encryption/decryption on the Android keystore and retrieve it using the 
user's fingerprint. 

<!--more-->

The use case I have in mind is the user login. My goal is to provide an
alternative to the username/password form with a biometric dialog. After the
user's first successful log in, I'll encrypt the user's password and keep
it my app's local storage. The key for encryption/decryption will be stored
in the Android keystore and will require biometric authentication for access.

### Installation

To use the new library we have to include this line in the app's build.gradle `dependencies` block:

``` kotlin
// 1.1.0 is the latest stable version at the time of writing
implementation("androidx.biometric:biometric:1.1.0")
 ``` 

### Check that biometric authentication is available

Although the new library supports up to API 23, not all devices actually have
biometric features. Before attempting anything, we must check if biometric
authentication is available using `BiometricManager.canAuthenticate()`:

``` kotlin
val biometricManager = BiometricManager.from(context)
return when (val result =
    biometricManager.canAuthenticate(BIOMETRIC_STRONG)) {
    BiometricManager.BIOMETRIC_SUCCESS -> 
        TODO("start biometric authentication")

    BiometricManager.BIOMETRIC_ERROR_NONE_ENROLLED -> {
        TODO("take user to biometric enrollment")
    }

    else -> TODO("biometric authentication not available. Show error message or skip")
}
```

We pass the [`BIOMETRIC_STRONG`](
https://developer.android.com/reference/androidx/biometric/BiometricManager.Authenticators#BIOMETRIC_STRONG()
) flag because fingerprint is considered strong
authentication. The keystore does not allow weak authentication for accessing keys.

This method returns different status codes that we have to handle. If the result
is `BIOMETRIC_SUCCESS`, we are good to go. If the result is
`BIOMETRIC_ERROR_NONE_ENROLLED`, the user needs to enroll a fingerprint on system 
settings. You can launch an activity for that, but it's only available on API 30:

``` kotlin
val intent = Intent(Settings.ACTION_BIOMETRIC_ENROLL).apply {
    putExtra(
        Settings.EXTRA_BIOMETRIC_AUTHENTICATORS_ALLOWED,
        BIOMETRIC_STRONG
    )
}
startActivity(intent)
```

### Generate a secret key in the Android keystore

Once we have determined the user's device is ready for biometric 
authentication, we need to generate a secret key for encryption/decryption.
To create it on the Android keystore, we need an instance of 
[`KeyGenParameterSpec`:](
https://developer.android.com/reference/android/security/keystore/KeyGenParameterSpec
)

``` kotlin
const val KEY_SIZE = 256
const val SECRET_KEY_NAME = "mySecretKeyName"

KeyGenParameterSpec.Builder(
    SECRET_KEY_NAME,
    KeyProperties.PURPOSE_ENCRYPT or KeyProperties.PURPOSE_DECRYPT
)
    .setKeySize(KEY_SIZE)
    .setBlockModes(KeyProperties.BLOCK_MODE_CBC)
    .setEncryptionPaddings(KeyProperties.ENCRYPTION_PADDING_PKCS7)
    .setUserAuthenticationRequired(true)
    .build()
```

This builder object defines the following things about our key:

- It can encrypt and decrypt.
- Its size is 256 bytes.
- It uses [Cipher Block Chaining (CBC)](
https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation) block mode.
- It uses [PKCS7](https://node-security.com/posts/cryptography-pkcs-7-padding/)
encryption padding scheme.
- Requires user authentication for access. (This is essential for biometric
authentication)

Then you can use [`KeyGenerator`](
https://developer.android.com/reference/kotlin/javax/crypto/KeyGenerator.html?hl=en
) to generate it: 

``` kotlin
val keyGenerator = KeyGenerator.getInstance(
    KeyProperties.KEY_ALGORITHM_AES, "AndroidKeyStore"
)
keyGenerator.init(keyGenParameterSpec)
keyGenerator.generateKey()
```

### Encrypt the user password with the new key

Now that the key is ready, we can use it to create a [`Cipher`](
https://developer.android.com/reference/kotlin/javax/crypto/Cipher?hl=en
) object that can encrypt the user password with [AES](
https://en.wikipedia.org/wiki/Advanced_Encryption_Standard). Passwords
should never be stored as plain text. We should wait until the user
authenticates with their password, grab it, and then create our cipher:

``` kotlin
const val SECRET_KEY_NAME = "mySecretKeyName"

private fun getSecretKey(): SecretKey {
    val keyStore = KeyStore.getInstance("AndroidKeyStore")

    // Before the keystore can be accessed, it must be loaded.
    keyStore.load(null)
    return keyStore.getKey(SECRET_KEY_NAME, null) as SecretKey
}

private fun getCipher(): Cipher {
    return Cipher.getInstance(
        KeyProperties.KEY_ALGORITHM_AES + "/"
                + KeyProperties.BLOCK_MODE_CBC + "/"
                + KeyProperties.ENCRYPTION_PADDING_PKCS7
    )
}

val cipher = getCipher()
val secretKey = getSecretKey(secretKeyName)
cipher.init(Cipher.ENCRYPT_MODE, secretKey)
```

Please note that this cipher wont work until the user authenticates. Remember
that `.setUserAuthenticationRequired(true)` in the `KeyGenParameterSpec` 
builder? We have to pass the cipher to `BiometricPrompt` so that
we can use it after the user authenticates with their fingerprint.

``` kotlin
val executor = ContextCompat.getMainExecutor(activity)

val biometricPrompt = BiometricPrompt(activity, executor,
    object : BiometricPrompt.AuthenticationCallback() {

        override fun onAuthenticationFailed() {
        }

        override fun onAuthenticationError(
            errorCode: Int,
            errString: CharSequence
        ) {
            handleError(errorCode, errString)
            Toast.makeText(this@MainActivity, errString, Toast.LENGTH_LONG).show()
            TODO("exit to next screen")
        }

        override fun onAuthenticationSucceeded(
            result: BiometricPrompt.AuthenticationResult
        ) {
            // cryptoObject cannot be null because we
            // explicitly pass it on authenticate()
            val resultCipher = result.cryptoObject!!.cipher
            encryptAndPersistPassword(resultCipher, password)
            TODO("exit to next screen")
        }

    })

val promptInfo = BiometricPrompt.PromptInfo.Builder()
    .setTitle("Biometric login for my app")
    .setSubtitle("Log in using your biometric credential")
    .setNegativeButtonText("Use account password")
    .setAllowedAuthenticators(BIOMETRIC_STRONG)
    .build()

biometricPrompt.authenticate(
    promptInfo,
    BiometricPrompt.CryptoObject(cipher)
)
```

Here we create a `BiometricPrompt` object,setting the callbacks for events that
occur while the user interacts with the dialog. `BiometricPrompt.authenticate()`
takes a `PromptInfo` object with the text that we want to display and a 
`CryptoObject` with our cipher to show the biometric authentication to the
user. Now let's talk about each callback:

In the `onAuthenticationFailed()` callback we don't really need to do anything.
This gets called every time the fingerprint does not match. The dialog shows this
error and lets the user retry multiple times before cancelling the operation. 

`onAuthenticationError()` is for errors that cancel the operation. Maybe the user
exceeded the maximum number of attempts, or it got cancelled by user action. Most
of the time we can just show the error to the user and exit, but there are a few
errors that should be handled differently. You can see this in `handleError()`:

``` kotlin
private fun handleError(errorCode: Int, errString: String) {
    if (errorCode == ERROR_NEGATIVE_BUTTON) {
        // User clicked negative button to close the dialog
        TODO("exit to next screen")
        return
    }

    if (errorCode == ERROR_USER_CANCELED) {
        // User clicked outside the dialog to close it
        TODO("exit to next screen")
        return
    }

    // Show the error to the user
    Toast.makeText(this@MainActivity, errString, Toast.LENGTH_LONG).show()
    TODO("exit to next screen")
}
```

`onAuthenticationSucceeded()` is where we get our cipher back with a valid key
and encrypt the password. Here's the implementation of
`encryptAndPersistPassword()`:

``` kotlin
private fun encryptAndPersistPassword(cipher: Cipher, password: String) {
    private val defaultCharset = Charset.forName("UTF-8")

    val encryptedBytes = resultCipher.doFinal(
        secret.toByteArray(defaultCharset)
    )
    val encryptedString = encryptedBytes.toBase64String()
    val ivString = resultCipher.iv.toBase64String()

    TODO("persist encryptedString and ivString")
}
```

After encrypting there are two byte arrays that we need to persist: the password
and the [initialization vector (IV)](
https://en.wikipedia.org/wiki/Initialization_vector). The cipher won't be able to
decrypt the password unless you provide both. For persisting you could use SQLite
or just [`SharedPreferences`](
https://developer.android.com/training/data-storage/shared-preferences
). It's far easier to persist strings than byte arrays, so I convert them to
[Base64](https://en.wikipedia.org/wiki/Base64) strings. These are the extension 
methods that I use:

```
fun ByteArray.toBase64String() = Base64.encodeToString(this, Base64.DEFAULT)

fun String.toBase64ByteArray() = Base64.decode(this, Base64.DEFAULT)
```

### Decrypt the user password

The next time the user wants to sign in we can just decrypt the password and 
instantly authenticate with the server. Once again, we have to create a
`Cipher` object, but this time in `DECRYPT_MODE` and pass the IV we persisted at
the end of the previous section:

``` kotlin
val cipher = getCipher()
val secretKey = getSecretKey()

val ivArray = ivString.toBase64ByteArray()
cipher.init(Cipher.DECRYPT_MODE, secretKey, IvParameterSpec(ivArray))
```

Then, similar to the previous section, we have to create a `BiometricPrompt` 
instance and call `authenticate()`:

``` kotlin
val promptInfo = BiometricPrompt.PromptInfo.Builder()
    .setTitle("Biometric login for my app")
    .setSubtitle("Log in using your biometric credential")
    .setNegativeButtonText("Use account password")
    .build()

biometricPrompt.authenticate(
    promptInfo,
    BiometricPrompt.CryptoObject(cipher)
)
``` 

The `BiometricPrompt` callback implementations are pretty similar to last time.
Error handling on `onAuthenticationError()` is bit different because
`ERROR_NEGATIVE_BUTTON` now has to revert the UI to the old username/password form.

```
if (errorCode == ERROR_NEGATIVE_BUTTON) {
    // User clicked negative button to close the dialog
    TODO("revert to username/password form")
    return
}

if (errorCode == ERROR_USER_CANCELED) {
    // User clicked outside the dialog to close it
    return
}

// Show the error to the user
Toast.makeText(this@MainActivity, errString, Toast.LENGTH_LONG).show()
TODO("exit to next screen")
```

Finally, the code to decrypt the password on `onAuthenticationSucceeded()`
is very straightforward:

``` kotlin
val decryptedPassword = resultCipher.doFinal(
    encryptedString.toBase64ByteArray()
)
TODO("log in with the decrypted password")
 ```

And that's it. We now have biometric authentication in our app. The new biometric
prompt has great UX. If something goes wrong, the user can always fallback to 
the username/password form.
