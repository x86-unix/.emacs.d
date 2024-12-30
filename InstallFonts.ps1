# 現在のユーザーのプロファイルパスを取得
$userProfile = [System.Environment]::GetFolderPath('UserProfile')

# ソースディレクトリとフォルダパスを生成
$SourceDir   = "$userProfile\AppData\Roaming\"
$Source      = "$userProfile\AppData\Roaming\*.ttf"
$Destination = (New-Object -ComObject Shell.Application).Namespace(0x14)
$TempFolder  = "C:\Windows\Temp\Fonts"

# ソースディレクトリの作成
New-Item -ItemType Directory -Force -Path $SourceDir

# 一時フォルダの作成
New-Item $TempFolder -Type Directory -Force | Out-Null

# フォントファイルの取得
$fontFiles = Get-ChildItem -Path $Source -Include '*.ttf','*.ttc','*.otf' -Recurse

foreach ($fontFile in $fontFiles) {
    $fontName = $fontFile.Name
    $fontPath = "C:\Windows\Fonts\$fontName"

    # フォントがすでにインストールされているか確認
    if (-not(Test-Path $fontPath)) {
        try {
            # フォントを一時フォルダにコピー
            $tempFontPath = "$TempFolder\$fontName"
            Copy-Item $fontFile.FullName -Destination $tempFontPath -ErrorAction Stop

            # フォントをインストール（ダイアログを表示しない）
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
