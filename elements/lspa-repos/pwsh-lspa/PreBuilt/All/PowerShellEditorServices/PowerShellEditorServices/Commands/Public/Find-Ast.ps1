#
# Copyright (c) Microsoft. All rights reserved.
# Licensed under the MIT license. See LICENSE file in the project root for full license information.
#

function Find-Ast {
    <#
    .EXTERNALHELP ..\PowerShellEditorServices.Commands-help.xml
    #>
    [CmdletBinding(PositionalBinding=$false, DefaultParameterSetName='FilterScript')]
    param(
        [Parameter(Position=0, ParameterSetName='FilterScript')]
        [ValidateNotNullOrEmpty()]
        [scriptblock]
        $FilterScript = { $true },

        [Parameter(ValueFromPipeline, ValueFromPipelineByPropertyName, ParameterSetName='FilterScript')]
        [ValidateNotNullOrEmpty()]
        [System.Management.Automation.Language.Ast]
        $Ast,

        [Parameter(ParameterSetName='FilterScript')]
        [switch]
        $Before,

        [Parameter(ParameterSetName='FilterScript')]
        [switch]
        $Family,

        [Parameter(ParameterSetName='FilterScript')]
        [Alias('Closest', 'F')]
        [switch]
        $First,

        [Parameter(ParameterSetName='FilterScript')]
        [Alias('Furthest')]
        [switch]
        $Last,

        [Parameter(ParameterSetName='FilterScript')]
        [Alias('Parent')]
        [switch]
        $Ancestor,

        [Parameter(ParameterSetName='FilterScript')]
        [switch]
        $IncludeStartingAst,

        [Parameter(ParameterSetName='AtCursor')]
        [switch]
        $AtCursor
    )
    begin {
        # InvokeWithContext method is PS4+, but it's significantly faster for large files.
        if ($PSVersionTable.PSVersion.Major -ge 4) {

            $variableType = [System.Management.Automation.PSVariable]
            function InvokeWithContext {
                param([scriptblock]$Filter, [System.Management.Automation.Language.Ast]$DollarUnder)

                return $Filter.InvokeWithContext(
                        <# functionsToDefine: #> $null,
                        <# variablesToDefine: #> [Activator]::CreateInstance($variableType, @('_', $DollarUnder)),
                        <# args:              #> $aAst)
            }
        } else {
            $FilterScript = [scriptblock]::Create($FilterScript.ToString())
            function InvokeWithContext {
                param([scriptblock]$Filter, [System.Management.Automation.Language.Ast]$DollarUnder)

                return $DollarUnder | & { process { $Filter.InvokeReturnAsIs($DollarUnder) } }
            }
        }
        # Get all children or ancestors.
        function GetAllFamily {
            param($Start)

            if ($Before.IsPresent) {
                $parent = $Start
                for ($parent; $parent = $parent.Parent) { $parent }
                return
            }
            return $Start.FindAll({ $true }, $true)
        }
        # Get all asts regardless of structure, in either direction from the starting ast.
        function GetAllAsts {
            param($Start)

            $predicate = [Func[System.Management.Automation.Language.Ast,bool]]{
                $args[0] -ne $Ast
            }

            $topParent = Find-Ast -Ast $Start -Ancestor -Last -IncludeStartingAst
            if (-not $topParent) { $topParent = $Start }

            if ($Before.IsPresent) {
                # Need to store so we can reverse the collection.
                $result = [Linq.Enumerable]::TakeWhile(
                    $topParent.FindAll({ $true }, $true),
                    $predicate) -as [System.Management.Automation.Language.Ast[]]

                [array]::Reverse($result)
                return $result
            }
            return [Linq.Enumerable]::SkipWhile(
                $topParent.FindAll({ $true }, $true),
                $predicate)
        }
    }
    process {
        if ($Ancestor.IsPresent) {
            $Family = $Before = $true
        }
        $context = $psEditor.GetEditorContext()

        if (-not $Ast -and $context) {
            $Ast = $context.CurrentFile.Ast
        }
        switch ($PSCmdlet.ParameterSetName) {
            AtCursor {
                $cursorLine     = $context.CursorPosition.Line - 1
                $cursorColumn   = $context.CursorPosition.Column - 1
                $cursorOffset   = $Ast.Extent.Text |
                    Select-String "(.*\r?\n){$cursorLine}.{$cursorColumn}" |
                    ForEach-Object { $PSItem.Matches.Value.Length }

                # yield
                Find-Ast -Last {
                    $cursorOffset -ge $PSItem.Extent.StartOffset -and
                    $cursorOffset -le $PSItem.Extent.EndOffset
                }
            }
            FilterScript {
                if (-not $Ast) { return }

                # Check if we're trying to get the top level ancestor when we're already there.
                if ($Before.IsPresent -and
                    $Family.IsPresent -and
                    $Last.IsPresent   -and -not
                    $Ast.Parent       -and
                    $Ast -is [System.Management.Automation.Language.ScriptBlockAst])
                    { return $Ast }

                if ($Family.IsPresent) {
                    $asts = GetAllFamily $Ast
                } else {
                    $asts = GetAllAsts $Ast
                }
                # Check the first ast to see if it's our starting ast, unless
                $checkFirstAst = -not $IncludeStartingAst
                foreach ($aAst in $asts) {
                    if ($checkFirstAst) {
                        if ($aAst -eq $Ast) {
                            $checkFirstAst = $false
                            continue
                        }
                    }
                    $shouldReturn = InvokeWithContext $FilterScript $aAst

                    if (-not $shouldReturn) { continue }

                    # Return first, last, both, or all depending on the combination of switches.
                    if (-not $Last.IsPresent) {
                        $aAst # yield
                        if ($First.IsPresent) { break }
                    } else {
                        $lastMatch = $aAst
                        if ($First.IsPresent) {
                            $aAst # yield
                            $First = $false
                        }
                    }
                }
                # yield
                if ($Last.IsPresent) { return $lastMatch }
            }
        }
    }
}

# SIG # Begin signature block
# MIIkVwYJKoZIhvcNAQcCoIIkSDCCJEQCAQExDzANBglghkgBZQMEAgEFADB5Bgor
# BgEEAYI3AgEEoGswaTA0BgorBgEEAYI3AgEeMCYCAwEAAAQQH8w7YFlLCE63JNLG
# KX7zUQIBAAIBAAIBAAIBAAIBADAxMA0GCWCGSAFlAwQCAQUABCBU+U+1PxmI6hqC
# Y4sXAdOFw6Eiwd9LViwum8AB6haEqqCCDYEwggX/MIID56ADAgECAhMzAAABUZ6N
# j0Bxow5BAAAAAAFRMA0GCSqGSIb3DQEBCwUAMH4xCzAJBgNVBAYTAlVTMRMwEQYD
# VQQIEwpXYXNoaW5ndG9uMRAwDgYDVQQHEwdSZWRtb25kMR4wHAYDVQQKExVNaWNy
# b3NvZnQgQ29ycG9yYXRpb24xKDAmBgNVBAMTH01pY3Jvc29mdCBDb2RlIFNpZ25p
# bmcgUENBIDIwMTEwHhcNMTkwNTAyMjEzNzQ2WhcNMjAwNTAyMjEzNzQ2WjB0MQsw
# CQYDVQQGEwJVUzETMBEGA1UECBMKV2FzaGluZ3RvbjEQMA4GA1UEBxMHUmVkbW9u
# ZDEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMR4wHAYDVQQDExVNaWNy
# b3NvZnQgQ29ycG9yYXRpb24wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIB
# AQCVWsaGaUcdNB7xVcNmdfZiVBhYFGcn8KMqxgNIvOZWNH9JYQLuhHhmJ5RWISy1
# oey3zTuxqLbkHAdmbeU8NFMo49Pv71MgIS9IG/EtqwOH7upan+lIq6NOcw5fO6Os
# +12R0Q28MzGn+3y7F2mKDnopVu0sEufy453gxz16M8bAw4+QXuv7+fR9WzRJ2CpU
# 62wQKYiFQMfew6Vh5fuPoXloN3k6+Qlz7zgcT4YRmxzx7jMVpP/uvK6sZcBxQ3Wg
# B/WkyXHgxaY19IAzLq2QiPiX2YryiR5EsYBq35BP7U15DlZtpSs2wIYTkkDBxhPJ
# IDJgowZu5GyhHdqrst3OjkSRAgMBAAGjggF+MIIBejAfBgNVHSUEGDAWBgorBgEE
# AYI3TAgBBggrBgEFBQcDAzAdBgNVHQ4EFgQUV4Iarkq57esagu6FUBb270Zijc8w
# UAYDVR0RBEkwR6RFMEMxKTAnBgNVBAsTIE1pY3Jvc29mdCBPcGVyYXRpb25zIFB1
# ZXJ0byBSaWNvMRYwFAYDVQQFEw0yMzAwMTIrNDU0MTM1MB8GA1UdIwQYMBaAFEhu
# ZOVQBdOCqhc3NyK1bajKdQKVMFQGA1UdHwRNMEswSaBHoEWGQ2h0dHA6Ly93d3cu
# bWljcm9zb2Z0LmNvbS9wa2lvcHMvY3JsL01pY0NvZFNpZ1BDQTIwMTFfMjAxMS0w
# Ny0wOC5jcmwwYQYIKwYBBQUHAQEEVTBTMFEGCCsGAQUFBzAChkVodHRwOi8vd3d3
# Lm1pY3Jvc29mdC5jb20vcGtpb3BzL2NlcnRzL01pY0NvZFNpZ1BDQTIwMTFfMjAx
# MS0wNy0wOC5jcnQwDAYDVR0TAQH/BAIwADANBgkqhkiG9w0BAQsFAAOCAgEAWg+A
# rS4Anq7KrogslIQnoMHSXUPr/RqOIhJX+32ObuY3MFvdlRElbSsSJxrRy/OCCZdS
# se+f2AqQ+F/2aYwBDmUQbeMB8n0pYLZnOPifqe78RBH2fVZsvXxyfizbHubWWoUf
# NW/FJlZlLXwJmF3BoL8E2p09K3hagwz/otcKtQ1+Q4+DaOYXWleqJrJUsnHs9UiL
# crVF0leL/Q1V5bshob2OTlZq0qzSdrMDLWdhyrUOxnZ+ojZ7UdTY4VnCuogbZ9Zs
# 9syJbg7ZUS9SVgYkowRsWv5jV4lbqTD+tG4FzhOwcRQwdb6A8zp2Nnd+s7VdCuYF
# sGgI41ucD8oxVfcAMjF9YX5N2s4mltkqnUe3/htVrnxKKDAwSYliaux2L7gKw+bD
# 1kEZ/5ozLRnJ3jjDkomTrPctokY/KaZ1qub0NUnmOKH+3xUK/plWJK8BOQYuU7gK
# YH7Yy9WSKNlP7pKj6i417+3Na/frInjnBkKRCJ/eYTvBH+s5guezpfQWtU4bNo/j
# 8Qw2vpTQ9w7flhH78Rmwd319+YTmhv7TcxDbWlyteaj4RK2wk3pY1oSz2JPE5PNu
# Nmd9Gmf6oePZgy7Ii9JLLq8SnULV7b+IP0UXRY9q+GdRjM2AEX6msZvvPCIoG0aY
# HQu9wZsKEK2jqvWi8/xdeeeSI9FN6K1w4oVQM4Mwggd6MIIFYqADAgECAgphDpDS
# AAAAAAADMA0GCSqGSIb3DQEBCwUAMIGIMQswCQYDVQQGEwJVUzETMBEGA1UECBMK
# V2FzaGluZ3RvbjEQMA4GA1UEBxMHUmVkbW9uZDEeMBwGA1UEChMVTWljcm9zb2Z0
# IENvcnBvcmF0aW9uMTIwMAYDVQQDEylNaWNyb3NvZnQgUm9vdCBDZXJ0aWZpY2F0
# ZSBBdXRob3JpdHkgMjAxMTAeFw0xMTA3MDgyMDU5MDlaFw0yNjA3MDgyMTA5MDla
# MH4xCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpXYXNoaW5ndG9uMRAwDgYDVQQHEwdS
# ZWRtb25kMR4wHAYDVQQKExVNaWNyb3NvZnQgQ29ycG9yYXRpb24xKDAmBgNVBAMT
# H01pY3Jvc29mdCBDb2RlIFNpZ25pbmcgUENBIDIwMTEwggIiMA0GCSqGSIb3DQEB
# AQUAA4ICDwAwggIKAoICAQCr8PpyEBwurdhuqoIQTTS68rZYIZ9CGypr6VpQqrgG
# OBoESbp/wwwe3TdrxhLYC/A4wpkGsMg51QEUMULTiQ15ZId+lGAkbK+eSZzpaF7S
# 35tTsgosw6/ZqSuuegmv15ZZymAaBelmdugyUiYSL+erCFDPs0S3XdjELgN1q2jz
# y23zOlyhFvRGuuA4ZKxuZDV4pqBjDy3TQJP4494HDdVceaVJKecNvqATd76UPe/7
# 4ytaEB9NViiienLgEjq3SV7Y7e1DkYPZe7J7hhvZPrGMXeiJT4Qa8qEvWeSQOy2u
# M1jFtz7+MtOzAz2xsq+SOH7SnYAs9U5WkSE1JcM5bmR/U7qcD60ZI4TL9LoDho33
# X/DQUr+MlIe8wCF0JV8YKLbMJyg4JZg5SjbPfLGSrhwjp6lm7GEfauEoSZ1fiOIl
# XdMhSz5SxLVXPyQD8NF6Wy/VI+NwXQ9RRnez+ADhvKwCgl/bwBWzvRvUVUvnOaEP
# 6SNJvBi4RHxF5MHDcnrgcuck379GmcXvwhxX24ON7E1JMKerjt/sW5+v/N2wZuLB
# l4F77dbtS+dJKacTKKanfWeA5opieF+yL4TXV5xcv3coKPHtbcMojyyPQDdPweGF
# RInECUzF1KVDL3SV9274eCBYLBNdYJWaPk8zhNqwiBfenk70lrC8RqBsmNLg1oiM
# CwIDAQABo4IB7TCCAekwEAYJKwYBBAGCNxUBBAMCAQAwHQYDVR0OBBYEFEhuZOVQ
# BdOCqhc3NyK1bajKdQKVMBkGCSsGAQQBgjcUAgQMHgoAUwB1AGIAQwBBMAsGA1Ud
# DwQEAwIBhjAPBgNVHRMBAf8EBTADAQH/MB8GA1UdIwQYMBaAFHItOgIxkEO5FAVO
# 4eqnxzHRI4k0MFoGA1UdHwRTMFEwT6BNoEuGSWh0dHA6Ly9jcmwubWljcm9zb2Z0
# LmNvbS9wa2kvY3JsL3Byb2R1Y3RzL01pY1Jvb0NlckF1dDIwMTFfMjAxMV8wM18y
# Mi5jcmwwXgYIKwYBBQUHAQEEUjBQME4GCCsGAQUFBzAChkJodHRwOi8vd3d3Lm1p
# Y3Jvc29mdC5jb20vcGtpL2NlcnRzL01pY1Jvb0NlckF1dDIwMTFfMjAxMV8wM18y
# Mi5jcnQwgZ8GA1UdIASBlzCBlDCBkQYJKwYBBAGCNy4DMIGDMD8GCCsGAQUFBwIB
# FjNodHRwOi8vd3d3Lm1pY3Jvc29mdC5jb20vcGtpb3BzL2RvY3MvcHJpbWFyeWNw
# cy5odG0wQAYIKwYBBQUHAgIwNB4yIB0ATABlAGcAYQBsAF8AcABvAGwAaQBjAHkA
# XwBzAHQAYQB0AGUAbQBlAG4AdAAuIB0wDQYJKoZIhvcNAQELBQADggIBAGfyhqWY
# 4FR5Gi7T2HRnIpsLlhHhY5KZQpZ90nkMkMFlXy4sPvjDctFtg/6+P+gKyju/R6mj
# 82nbY78iNaWXXWWEkH2LRlBV2AySfNIaSxzzPEKLUtCw/WvjPgcuKZvmPRul1LUd
# d5Q54ulkyUQ9eHoj8xN9ppB0g430yyYCRirCihC7pKkFDJvtaPpoLpWgKj8qa1hJ
# Yx8JaW5amJbkg/TAj/NGK978O9C9Ne9uJa7lryft0N3zDq+ZKJeYTQ49C/IIidYf
# wzIY4vDFLc5bnrRJOQrGCsLGra7lstnbFYhRRVg4MnEnGn+x9Cf43iw6IGmYslmJ
# aG5vp7d0w0AFBqYBKig+gj8TTWYLwLNN9eGPfxxvFX1Fp3blQCplo8NdUmKGwx1j
# NpeG39rz+PIWoZon4c2ll9DuXWNB41sHnIc+BncG0QaxdR8UvmFhtfDcxhsEvt9B
# xw4o7t5lL+yX9qFcltgA1qFGvVnzl6UJS0gQmYAf0AApxbGbpT9Fdx41xtKiop96
# eiL6SJUfq/tHI4D1nvi/a7dLl+LrdXga7Oo3mXkYS//WsyNodeav+vyL6wuA6mk7
# r/ww7QRMjt/fdW1jkT3RnVZOT7+AVyKheBEyIXrvQQqxP/uozKRdwaGIm1dxVk5I
# RcBCyZt2WwqASGv9eZ/BvW1taslScxMNelDNMYIWLDCCFigCAQEwgZUwfjELMAkG
# A1UEBhMCVVMxEzARBgNVBAgTCldhc2hpbmd0b24xEDAOBgNVBAcTB1JlZG1vbmQx
# HjAcBgNVBAoTFU1pY3Jvc29mdCBDb3Jwb3JhdGlvbjEoMCYGA1UEAxMfTWljcm9z
# b2Z0IENvZGUgU2lnbmluZyBQQ0EgMjAxMQITMwAAAVGejY9AcaMOQQAAAAABUTAN
# BglghkgBZQMEAgEFAKCBrjAZBgkqhkiG9w0BCQMxDAYKKwYBBAGCNwIBBDAcBgor
# BgEEAYI3AgELMQ4wDAYKKwYBBAGCNwIBFTAvBgkqhkiG9w0BCQQxIgQghurit13E
# oZm6jsVT8BnAwO1/PEYjbK6rJwdU2Ar49RMwQgYKKwYBBAGCNwIBDDE0MDKgFIAS
# AE0AaQBjAHIAbwBzAG8AZgB0oRqAGGh0dHA6Ly93d3cubWljcm9zb2Z0LmNvbTAN
# BgkqhkiG9w0BAQEFAASCAQBXTmxDkMKIWHZX822lM0HkV1eeb/Z93bGn7/D6vqrW
# rVf5pBpBjDOKioBjqwx/1684vhM93nXMezymhyJSiy4XeaZ3y1a5vq40y9XPZntc
# uw1aIWDr93HXb1x+2RgYY6k021aCC3eXrMeoGY4Bfjc/IKLMhlzUBac8D02RG8n7
# u2IrQ8LhjrNIRiJ9uIuPYjjZgmMNRHgQKFYnGpUwGJGnoOcpPupb7EeJVdxTXHaH
# jK6P6l8+J7zInc15sZx3EE/XfBV6DtMxC3T/PzcgJi5vffePg1QXvbRdfqSdlB/x
# 6nhksvrqHf17qjIEzoCs2uSTXX9NE30mzAC26vV1033woYITtjCCE7IGCisGAQQB
# gjcDAwExghOiMIITngYJKoZIhvcNAQcCoIITjzCCE4sCAQMxDzANBglghkgBZQME
# AgEFADCCAVQGCyqGSIb3DQEJEAEEoIIBQwSCAT8wggE7AgEBBgorBgEEAYRZCgMB
# MDEwDQYJYIZIAWUDBAIBBQAEIM+Vl32a9kYoj8U0zRGQGsukogsv9Sc2TbI24RSW
# EKdjAgZeS1PU2aEYEjIwMjAwMzA1MTcwNjA1LjYyWjAEgAIB9KCB1KSB0TCBzjEL
# MAkGA1UEBhMCVVMxEzARBgNVBAgTCldhc2hpbmd0b24xEDAOBgNVBAcTB1JlZG1v
# bmQxHjAcBgNVBAoTFU1pY3Jvc29mdCBDb3Jwb3JhdGlvbjEpMCcGA1UECxMgTWlj
# cm9zb2Z0IE9wZXJhdGlvbnMgUHVlcnRvIFJpY28xJjAkBgNVBAsTHVRoYWxlcyBU
# U1MgRVNOOjE0OEMtQzRCOS0yMDY2MSUwIwYDVQQDExxNaWNyb3NvZnQgVGltZS1T
# dGFtcCBTZXJ2aWNloIIPIjCCBPUwggPdoAMCAQICEzMAAAEGYbVEmfjq8u0AAAAA
# AQYwDQYJKoZIhvcNAQELBQAwfDELMAkGA1UEBhMCVVMxEzARBgNVBAgTCldhc2hp
# bmd0b24xEDAOBgNVBAcTB1JlZG1vbmQxHjAcBgNVBAoTFU1pY3Jvc29mdCBDb3Jw
# b3JhdGlvbjEmMCQGA1UEAxMdTWljcm9zb2Z0IFRpbWUtU3RhbXAgUENBIDIwMTAw
# HhcNMTkwOTA2MjA0MTE5WhcNMjAxMjA0MjA0MTE5WjCBzjELMAkGA1UEBhMCVVMx
# EzARBgNVBAgTCldhc2hpbmd0b24xEDAOBgNVBAcTB1JlZG1vbmQxHjAcBgNVBAoT
# FU1pY3Jvc29mdCBDb3Jwb3JhdGlvbjEpMCcGA1UECxMgTWljcm9zb2Z0IE9wZXJh
# dGlvbnMgUHVlcnRvIFJpY28xJjAkBgNVBAsTHVRoYWxlcyBUU1MgRVNOOjE0OEMt
# QzRCOS0yMDY2MSUwIwYDVQQDExxNaWNyb3NvZnQgVGltZS1TdGFtcCBTZXJ2aWNl
# MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEArr9jwWp0S7dzOEOhiC86
# flHg/y8uMNR76LWEuelieQtTLK6qbd1q+1pgkJTmOohyfCIuq1tOCgqJOfguzcQR
# 41p2KYUm7qKP5+hbGEqcC1oznZxSYk7Ftxc6Sv2cN7g+zB0IF392TPyADRujyD7r
# NC9nPorjgS0Gh/7TJOCXU70bIaarnFIIXEuwnOZckTuRtb+ICoVyX9anc0Mh+YYf
# fzkfYg8pKM5EOqzrVzZDjf3tm0EX1FT/Wvvvf9KHGJ7cpoHamJvehvqvvM+NfPrM
# cZwtdq0B5BVjzi6UYwdZT/cQ49xeNlK5oDMIjMaPVU/vknG6l9zt8/r/LgqaLcCZ
# ZwIDAQABo4IBGzCCARcwHQYDVR0OBBYEFPnUtdlfjVfnsQVZJgk0CehOb9NMMB8G
# A1UdIwQYMBaAFNVjOlyKMZDzQ3t8RhvFM2hahW1VMFYGA1UdHwRPME0wS6BJoEeG
# RWh0dHA6Ly9jcmwubWljcm9zb2Z0LmNvbS9wa2kvY3JsL3Byb2R1Y3RzL01pY1Rp
# bVN0YVBDQV8yMDEwLTA3LTAxLmNybDBaBggrBgEFBQcBAQROMEwwSgYIKwYBBQUH
# MAKGPmh0dHA6Ly93d3cubWljcm9zb2Z0LmNvbS9wa2kvY2VydHMvTWljVGltU3Rh
# UENBXzIwMTAtMDctMDEuY3J0MAwGA1UdEwEB/wQCMAAwEwYDVR0lBAwwCgYIKwYB
# BQUHAwgwDQYJKoZIhvcNAQELBQADggEBAI6ErWi124Hq21/7/Kvk+rPIMqLX/+VC
# c3DVgWSIZKZ3RcJPT5/M2TBmjKZdExonX3tr2MOt7FGN8LyzIQ9gKiwKVZ8zJpx3
# PJ7xALuj5KI1RwzsHYHuGS6U8pH3bXyJQBkrBvzq2VYWkLvbHOFmG3hTOvsxWsYp
# orVo4aDWRaVqj2+5rsv8PIl9wrUueNHB2ZPTP1UBCgZ6fnjYPP5A5IM2uPONAsDm
# QL/vgi3p2qKvpoMdBXK22kF4mk+Bl+ve1j9VHy4kgxXahC9+jPaYBeF3fgA4eCLC
# pGzyNZhaMUTF0sOOCh+e8na1MC3PQF032qCs9h1PrkrmKTGBgIzAYJ0wggZxMIIE
# WaADAgECAgphCYEqAAAAAAACMA0GCSqGSIb3DQEBCwUAMIGIMQswCQYDVQQGEwJV
# UzETMBEGA1UECBMKV2FzaGluZ3RvbjEQMA4GA1UEBxMHUmVkbW9uZDEeMBwGA1UE
# ChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMTIwMAYDVQQDEylNaWNyb3NvZnQgUm9v
# dCBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkgMjAxMDAeFw0xMDA3MDEyMTM2NTVaFw0y
# NTA3MDEyMTQ2NTVaMHwxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpXYXNoaW5ndG9u
# MRAwDgYDVQQHEwdSZWRtb25kMR4wHAYDVQQKExVNaWNyb3NvZnQgQ29ycG9yYXRp
# b24xJjAkBgNVBAMTHU1pY3Jvc29mdCBUaW1lLVN0YW1wIFBDQSAyMDEwMIIBIjAN
# BgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAqR0NvHcRijog7PwTl/X6f2mUa3RU
# ENWlCgCChfvtfGhLLF/Fw+Vhwna3PmYrW/AVUycEMR9BGxqVHc4JE458YTBZsTBE
# D/FgiIRUQwzXTbg4CLNC3ZOs1nMwVyaCo0UN0Or1R4HNvyRgMlhgRvJYR4YyhB50
# YWeRX4FUsc+TTJLBxKZd0WETbijGGvmGgLvfYfxGwScdJGcSchohiq9LZIlQYrFd
# /XcfPfBXday9ikJNQFHRD5wGPmd/9WbAA5ZEfu/QS/1u5ZrKsajyeioKMfDaTgaR
# togINeh4HLDpmc085y9Euqf03GS9pAHBIAmTeM38vMDJRF1eFpwBBU8iTQIDAQAB
# o4IB5jCCAeIwEAYJKwYBBAGCNxUBBAMCAQAwHQYDVR0OBBYEFNVjOlyKMZDzQ3t8
# RhvFM2hahW1VMBkGCSsGAQQBgjcUAgQMHgoAUwB1AGIAQwBBMAsGA1UdDwQEAwIB
# hjAPBgNVHRMBAf8EBTADAQH/MB8GA1UdIwQYMBaAFNX2VsuP6KJcYmjRPZSQW9fO
# mhjEMFYGA1UdHwRPME0wS6BJoEeGRWh0dHA6Ly9jcmwubWljcm9zb2Z0LmNvbS9w
# a2kvY3JsL3Byb2R1Y3RzL01pY1Jvb0NlckF1dF8yMDEwLTA2LTIzLmNybDBaBggr
# BgEFBQcBAQROMEwwSgYIKwYBBQUHMAKGPmh0dHA6Ly93d3cubWljcm9zb2Z0LmNv
# bS9wa2kvY2VydHMvTWljUm9vQ2VyQXV0XzIwMTAtMDYtMjMuY3J0MIGgBgNVHSAB
# Af8EgZUwgZIwgY8GCSsGAQQBgjcuAzCBgTA9BggrBgEFBQcCARYxaHR0cDovL3d3
# dy5taWNyb3NvZnQuY29tL1BLSS9kb2NzL0NQUy9kZWZhdWx0Lmh0bTBABggrBgEF
# BQcCAjA0HjIgHQBMAGUAZwBhAGwAXwBQAG8AbABpAGMAeQBfAFMAdABhAHQAZQBt
# AGUAbgB0AC4gHTANBgkqhkiG9w0BAQsFAAOCAgEAB+aIUQ3ixuCYP4FxAz2do6Eh
# b7Prpsz1Mb7PBeKp/vpXbRkws8LFZslq3/Xn8Hi9x6ieJeP5vO1rVFcIK1GCRBL7
# uVOMzPRgEop2zEBAQZvcXBf/XPleFzWYJFZLdO9CEMivv3/Gf/I3fVo/HPKZeUqR
# UgCvOA8X9S95gWXZqbVr5MfO9sp6AG9LMEQkIjzP7QOllo9ZKby2/QThcJ8ySif9
# Va8v/rbljjO7Yl+a21dA6fHOmWaQjP9qYn/dxUoLkSbiOewZSnFjnXshbcOco6I8
# +n99lmqQeKZt0uGc+R38ONiU9MalCpaGpL2eGq4EQoO4tYCbIjggtSXlZOz39L9+
# Y1klD3ouOVd2onGqBooPiRa6YacRy5rYDkeagMXQzafQ732D8OE7cQnfXXSYIghh
# 2rBQHm+98eEA3+cxB6STOvdlR3jo+KhIq/fecn5ha293qYHLpwmsObvsxsvYgrRy
# zR30uIUBHoD7G4kqVDmyW9rIDVWZeodzOwjmmC3qjeAzLhIp9cAvVCch98isTtoo
# uLGp25ayp0Kiyc8ZQU3ghvkqmqMRZjDTu3QyS99je/WZii8bxyGvWbWu3EQ8l1Bx
# 16HSxVXjad5XwdHeMMD9zOZN+w2/XU/pnR4ZOC+8z1gFLu8NoFA12u8JJxzVs341
# Hgi62jbb01+P3nSISRKhggOwMIICmAIBATCB/qGB1KSB0TCBzjELMAkGA1UEBhMC
# VVMxEzARBgNVBAgTCldhc2hpbmd0b24xEDAOBgNVBAcTB1JlZG1vbmQxHjAcBgNV
# BAoTFU1pY3Jvc29mdCBDb3Jwb3JhdGlvbjEpMCcGA1UECxMgTWljcm9zb2Z0IE9w
# ZXJhdGlvbnMgUHVlcnRvIFJpY28xJjAkBgNVBAsTHVRoYWxlcyBUU1MgRVNOOjE0
# OEMtQzRCOS0yMDY2MSUwIwYDVQQDExxNaWNyb3NvZnQgVGltZS1TdGFtcCBTZXJ2
# aWNloiUKAQEwCQYFKw4DAhoFAAMVAGO9+BoeVYVq7hbxu+WwvM9UNfL5oIHeMIHb
# pIHYMIHVMQswCQYDVQQGEwJVUzETMBEGA1UECBMKV2FzaGluZ3RvbjEQMA4GA1UE
# BxMHUmVkbW9uZDEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMSkwJwYD
# VQQLEyBNaWNyb3NvZnQgT3BlcmF0aW9ucyBQdWVydG8gUmljbzEnMCUGA1UECxMe
# bkNpcGhlciBOVFMgRVNOOjRERTktMEM1RS0zRTA5MSswKQYDVQQDEyJNaWNyb3Nv
# ZnQgVGltZSBTb3VyY2UgTWFzdGVyIENsb2NrMA0GCSqGSIb3DQEBBQUAAgUA4gut
# 1zAiGA8yMDIwMDMwNjAwNTY1NVoYDzIwMjAwMzA3MDA1NjU1WjB3MD0GCisGAQQB
# hFkKBAExLzAtMAoCBQDiC63XAgEAMAoCAQACAg0aAgH/MAcCAQACAhjUMAoCBQDi
# DP9XAgEAMDYGCisGAQQBhFkKBAIxKDAmMAwGCisGAQQBhFkKAwGgCjAIAgEAAgMH
# oSChCjAIAgEAAgMHoSAwDQYJKoZIhvcNAQEFBQADggEBAA24svaoAnG+hMDcnI4V
# sVkbUKEo3w8hcuOc3xqAEb3vZPdbcesehATHR+avfh2cs39uyrsUUgF5vyGdbU3C
# mfdpEnl4Hb12yL+wX92anehck26w4/pMjv30Cmdt5zZXwHE2SIdB021k9pqs+0+z
# +/TxktrMq/hLt1O3sCYKI+22ONzVOWNaSrrRGPNgzNT7ykQfCBABi/dHnAfKyQdK
# muHhIdaLeTDLrIh9jZ7K/QGTvKUPDbaSV0sq7ngRF/Yrp0/UzPhICjzR3qO8jBYa
# ifpMtOHkwev7S+fHqeNzbdP7CDxZTnPKMhG5EWq3ysOedgpT/lw18GKu2WSWQcye
# XG0xggL1MIIC8QIBATCBkzB8MQswCQYDVQQGEwJVUzETMBEGA1UECBMKV2FzaGlu
# Z3RvbjEQMA4GA1UEBxMHUmVkbW9uZDEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBv
# cmF0aW9uMSYwJAYDVQQDEx1NaWNyb3NvZnQgVGltZS1TdGFtcCBQQ0EgMjAxMAIT
# MwAAAQZhtUSZ+Ory7QAAAAABBjANBglghkgBZQMEAgEFAKCCATIwGgYJKoZIhvcN
# AQkDMQ0GCyqGSIb3DQEJEAEEMC8GCSqGSIb3DQEJBDEiBCD0uen4WhbqoPmqlom0
# kxkPWn9Rhl8Hp+fYV/D3WrQxKjCB4gYLKoZIhvcNAQkQAgwxgdIwgc8wgcwwgbEE
# FGO9+BoeVYVq7hbxu+WwvM9UNfL5MIGYMIGApH4wfDELMAkGA1UEBhMCVVMxEzAR
# BgNVBAgTCldhc2hpbmd0b24xEDAOBgNVBAcTB1JlZG1vbmQxHjAcBgNVBAoTFU1p
# Y3Jvc29mdCBDb3Jwb3JhdGlvbjEmMCQGA1UEAxMdTWljcm9zb2Z0IFRpbWUtU3Rh
# bXAgUENBIDIwMTACEzMAAAEGYbVEmfjq8u0AAAAAAQYwFgQU6EZngqUjrZPCTM4K
# TDWtKsOEJo8wDQYJKoZIhvcNAQELBQAEggEAMTlE2qYPSl4hCN1CsJwjORDVbWRF
# WF6h4np+fNg48L1aVm5YABjqorFXYsEYnfS4LSlspxXEBTNaL2Z2C8Hen75SGEs9
# EXmRTZeOkjVtWjLMGDvFsyT3Kar+/2qTwZgyVsdXyhsXQyWB7/MJT2opEmhWkIpA
# 4f48SF22CIdFOONmIC53ylAM28gxX/z2s+0FaZl4rQ1U7a6nf4cBPvhTrYmkhrB1
# M0kmuPE9yXIIxptORmO8VQ6ZllD5fr0DHEyUWUzNfmNokW9iHAJq5ay1lceCCG3C
# uBBnKFe6C1gfv4k+9+kYkPSg1LCd0gC8IUE3kttaPvOguZNBI5bmiDgNOA==
# SIG # End signature block
