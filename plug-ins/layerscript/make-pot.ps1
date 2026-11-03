# Define the path to add
pushd "$PSScriptRoot"
cd ..


$folderToAdd = "C:\Program Files\Poedit"
# Check if the folder already exists in the Path to avoid duplicates
if ($env:Path -notlike "*$folderToAdd*") {
    # Append the folder path to the current Path environment variable
    $env:Path = "$folderToAdd" +";" + $env:Path    
    Write-Host "'$folderToAdd' is added to `$`{env:PATH`}."
} else {
    Write-Host "'$folderToAdd' is already in `$`{env:PATH`}."
}

$folderToAdd = "C:\Program Files\gettext-iconv\bin"
if (-not $(test-path -path "$folderToAdd" -pathtype container)){
    $folderToAdd = "C:\Program Files\Poedit\GettextTools\bin"
}
# Check if the folder already exists in the Path to avoid duplicates
if ($env:Path -notlike "*$folderToAdd*") {
    # Append the folder path to the current Path environment variable
    $env:Path = "$folderToAdd" +";" + $env:Path
    Write-Host "'$folderToAdd' is added to `$`{env:PATH`}."
} else {
    Write-Host "'$folderToAdd' is already in `$`{env:PATH`}."
}


mkdir "locale\ko\LC_MESSAGES" -erroraction silentlycontinue

xgettext --force-po --no-wrap --language Scheme --from-code=UTF-8 -c -p locale -o "$((get-item $PWD).name).pot" "$((get-item $PWD).name).scm"
ls locale

$polong="ko"
$popath=".\locale\${polong}\LC_MESSAGES\$((get-item $PWD).name).po"
if (test-path -path $popath -type leaf) {
    poedit $popath
} else {
    poedit ".\locale\$((get-item $PWD).name).pot"
}


popd
