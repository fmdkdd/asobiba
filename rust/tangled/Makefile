NDK_HOME := /opt/android-ndk
PATH := /opt/android-ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/:/usr/bin

.PHONY: apk
apk:
	env NDK_HOME=$(NDK_HOME) PATH=$(PATH) cargo apk

.PHONY: apk-install
apk-install: apk
	adb install -r target/android-artifacts/build/bin/tangled-debug.apk

.PHONY: apk-run
apk-run: apk-install
	adb logcat -c
	adb logcat | rg 'Rust'
