android_run() {
    adb push $1 /data/local/tmp/temp.out > /dev/null
    adb shell chmod +x /data/local/tmp/temp.out > /dev/null
    adb shell ./data/local/tmp/temp.out
    adb shell rm /data/local/tmp/temp.out > /dev/null
}

android_run_for_exit_code() {
    adb push $1 /data/local/tmp/$1 > /dev/null
    adb shell chmod +x /data/local/tmp/$1 > /dev/null 2>&1
    adb shell "echo input | ./data/local/tmp/$1 > /dev/null 2>&1 ; echo \"\$?\"" 2>/dev/null
    adb shell rm /data/local/tmp/$1 > /dev/null 2>&1
}
