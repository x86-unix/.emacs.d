# 現在のユーザーのプロファイルパスを取得
$userProfile = [System.Environment]::GetFolderPath('UserProfile')

# ソースディレクトリとフォルダパスを生成
$SourceDir   = "$userProfile\AppData\Roaming\"
$Source      = "$userProfile\AppData\Roaming\*.ttf"
$TempFolder  = "C:\Windows\Temp\Fonts"

# ソースディレクトリの作成
New-Item -ItemType Directory -Force -Path $SourceDir

# 一時フォルダの作成
New-Item $TempFolder -Type Directory -Force | Out-Null

# フォントファイルの取得
$fontFiles = Get-ChildItem -Path $Source -Include '*.ttf','*.ttc','*.otf' -Recurse

# インストールされているフォントの名前を取得
$installedFonts = [System.Drawing.FontFamily]::Families | Select-Object -Property Name

# フォント名を取得する関数
function Get-FontNameFromFile {
    param (
        [string]$fontFilePath
    )
    $privateFontCollection = New-Object System.Drawing.Text.PrivateFontCollection
    $privateFontCollection.AddFontFile($fontFilePath)
    return $privateFontCollection.Families[0].Name
}

foreach ($fontFile in $fontFiles) {
    # フォント名をファイルから取得
    $fontName = Get-FontNameFromFile -fontFilePath $fontFile.FullName

    # フォント名を表示
    Write-Host "Font name from file: $fontName"

    # フォントがすでにインストールされているか確認
    $isInstalled = $installedFonts | Where-Object { $_.Name -eq $fontName }

    if (-not $isInstalled) {
        try {
            # フォントを一時フォルダにコピー
            $tempFontPath = "$TempFolder\$($fontFile.Name)"
            Copy-Item $fontFile.FullName -Destination $tempFontPath -ErrorAction Stop

            # フォントをインストール（ダイアログを表示しない）
            $Destination = (New-Object -ComObject Shell.Application).Namespace(0x14)
            $Destination.CopyHere($tempFontPath, 0x10 -bor 0x4000)

            # インストールが完了するまで待機
            Start-Sleep -Seconds 2

            # 成功メッセージを表示
            Write-Host "Successfully installed font: $fontName"
        } catch {
            Write-Host "Error installing font: $fontName. Error: $_"
        } finally {
            # 一時フォントファイルを削除
            Remove-Item -Path $tempFontPath -Force -ErrorAction SilentlyContinue
        }
    } else {
        Write-Host "Font already installed: $fontName"
    }
}
