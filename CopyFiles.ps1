# Prompt the user for input
$sourceDirectory = Read-Host "Enter the source directory path"
$destinationFileDir = Read-Host "Enter the destination dir"
$tile = Read-Host "Enter the tile number"
$pattern = '_\d{4}_NBR'

# Get a list of all files in the source directory
$allFiles = Get-ChildItem -Path $sourceDirectory

# Filter files that match both patterns
$matchingFiles = $allFiles | Where-Object {
    $_.Name -match $pattern -and $_.Name -match $tile
}
<# # Extract the numeric value from the pattern
$numericValue0 = [regex]::Match($matchingFiles, '_\d{4}_').Value
$numericValue = [regex]::Match($numericValue0, '\d{4}').Value

#$destinationFilePath = $destinationFileDir  + numericValue
$destinationFileName = "${numericValue}_Output.txt"
$destinationFilePath = Join-Path -Path $destinationFileDir -ChildPath $destinationFileName
 #>

# Create or overwrite the destination text files
$matchingFiles.GetEnumerator() | ForEach-Object {
	Write-Host "Matching files for numeric value '$_' "
	
    $numericValue0 = [regex]::Match($_, '_\d{4}_').Value
	$numericValue = [regex]::Match($numericValue0, '\d{4}').Value
	
	# Create the subdirectory name
	$subDirectoryName = $numericValue

	# Construct subdirectory path
	$subDirectoryPath = Join-Path -Path $destinationFileDir -ChildPath $subDirectoryName

	# Create the subdirectory if it doesn't exist
	if (-not (Test-Path $subDirectoryPath)) {
		New-Item -Path $subDirectoryPath -ItemType Directory
		Write-Host "Created subdirectory $subDirectoryPath"
	}
	
	# Copy the file to the subdirectory
	$destinationPath = Join-Path -Path $subDirectoryPath -ChildPath $file.Name
	Copy-Item -Path $_.FullName -Destination $destinationPath -Force
	Write-Host "Copied $($file.Name) to '$destinationPath' "
    

    Write-Host "Matching files for numeric value '$numericValue' have been written to $destinationPath."
}




<# # Create or overwrite the destination text file
$null = New-Item -Path $destinationFilePath -ItemType File

# Append matching file names to the destination text file
$matchingFiles | ForEach-Object { $_.Name | Out-File -Append -FilePath $destinationFilePath }
# Create or overwrite the destination text files
 #>
Write-Host "Matching files for numeric value '$numericValue' have been written to $destinationFilePath."

Write-Host "Matching files have been copied to the destination text file."
