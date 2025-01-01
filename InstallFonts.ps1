# 第一引数を取得。指定されていない場合はエラーメッセージを表示して終了
param (
    [string]$SourceDir
)

# スクリプト自身のファイル名を取得
$scriptName = $MyInvocation.MyCommand.Name

# 現在のユーザー名を取得
$currentUserName = [System.Environment]::GetFolderPath('UserProfile').Split('\')[-1]

if (-not $SourceDir) {
    Write-Host "エラー: ソースディレクトリを指定してください。" -ForegroundColor Red
    Write-Host "例: .\$scriptName 'C:\Users\$currentUserName\Documents\Fonts\'" -ForegroundColor Yellow
    Write-Host "または: .\$scriptName 'C:\Users\$currentUserName\AppData\Local\share\fonts\'" -ForegroundColor Yellow
    exit 1
}

# ソースファイルのパスを生成
$Source      = "$SourceDir*.ttf"
$TempFolder  = "C:\Windows\Temp\Fonts"

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
