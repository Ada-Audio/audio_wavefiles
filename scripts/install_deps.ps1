# Install-Module -Name GitHubActions

if ( Test-Path -Path '.\deps\audio_base' -PathType Container )
{
    "Using available component!"
}
else
{
    New-Item -Path ".\deps" -ItemType Directory

    # git clone --branch 20200910-refactoring https://github.com/Ada-Audio/audio_base.git ".\deps"
    Invoke-WebRequest -Uri https://github.com/Ada-Audio/audio_base/archive/feature/20200910-refactoring.zip -OutFile ".\audio_base.zip"
    Expand-Archive ".\audio_base.zip" -DestinationPath ".\deps"
    Rename-Item -Path ".\deps\audio_base-feature-20200910-refactoring" -NewName "audio_base"
}

[System.Environment]::SetEnvironmentVariable('GPR_PROJECT_PATH', -join((Get-Item .).FullName, '\deps\audio_base'))
