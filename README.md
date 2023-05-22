# Introduction
The following UDF can be used to compress a string into a binary via Huffman encoding.
To keep the overhead for the huff table integrated in the binary as small as possible, the canonical form of the huffman table was implemented.

The UDF accesses many functionalities of the UDF "BinaryHelpers.au3". This is also part of the repository and offers functionalities for simplified handling of binary data in AutoIt.

# How to use?
the following script:
```AutoIt
#include "Huffman.au3"

; read out the autoitscript.com page as example string
Global $sString = BinaryToString(InetRead("https://autoitscript.com"))
ConsoleWrite("Original Size: " & BinaryLen(StringToBinary($sString)) & " Bytes" & @CRLF)

; encode the string with a canonical huffman encoding
Global $bEncoded = _huff_encodeString($sString)
ConsoleWrite("Encoded Size:  " & BinaryLen($bEncoded) & " Bytes" & @CRLF & _
	"Compress ratio: " & Round((1 - (BinaryLen($bEncoded) / BinaryLen(StringToBinary($sString)))) * 100.0, 1) & ' %' & @CRLF)

; decode the string out of the binary
Global $sStringDecoded = _huff_decodeBinary($bEncoded)

; check if original and decoded string are the same
ConsoleWrite("$sString == $sStringDecoded: " & ($sString == $sStringDecoded) & @CRLF)
```
produces:
```
Original Size: 157081 Bytes
Encoded Size:  102833 Bytes
Compress ratio: 34.5 %
$sString == $sStringDecoded: True
```

## How to further increase the compression potential?
Huffman encoding is just a step to be able to compress data. In combination with further steps you can compress the data even more.
For example, you can pre-compress the data at the text level and only then encode the data using Huffman.

The following extends the above example with a (simple) word repetition compression:


<details>
<summary>Huffman encoding + word repetition string compression</summary>

```AutoIt
#include "Huffman.au3"

; read out the autoitscript.com page as example string
Global $sString = BinaryToString(InetRead("https://autoitscript.com"))
ConsoleWrite("Original Size: " & BinaryLen(StringToBinary($sString)) & " Bytes" & @CRLF)

; precompress at text level by word repetition:
Global $sStringCompressed = _strcmp_compressByWordRepetition($sString)

; encode the string with a canonical huffman encoding
Global $bEncoded = _huff_encodeString($sStringCompressed)
ConsoleWrite("Encoded Size:  " & BinaryLen($bEncoded) & " Bytes" & @CRLF & _
	"Compress ratio: " & Round((1 - (BinaryLen($bEncoded) / BinaryLen(StringToBinary($sString)))) * 100.0, 1) & ' %' & @CRLF)

; decode the string out of the binary
Global $sStringDecoded = _huff_decodeBinary($bEncoded)

; decompress at text level
$sStringDecoded = _strcmp_decompressByWordRepetition($sStringDecoded)

; check if original and decoded string are the same
ConsoleWrite("$sString == $sStringDecoded: " & ($sString == $sStringDecoded) & @CRLF)







; Tokenbasierte Kompression - ersetzt wiederholende Teile durch Nummern im Text
Func _strcmp_compressByWordRepetition($sString)
	Local 	$mTmp[3], $aTmp[2], $mPos, $iLen, _
			$mWords[], _
			$iPos = 1, _
			$sWord, $iWord = 0

	; Bereits vorhandene Präfixnummern escapen:
	$sString = StringRegExpReplace($sString, '(?s)\b(\d+)', Chr(27) & "$1")

	; Einzelne Wörter aus dem String extrahieren
	Do
		$aMatch = StringRegExp($sString, '(?s)\G.*?(\b[a-zA-ZäöüßÄÖÜ][\wäöüßÄÖÜ]{2,}\b)', 1, $iPos)
		IF @error Then ExitLoop
		$iPos = @extended
		$sWord = $aMatch[0]

		If MapExists($mWords, $sWord) Then
			$aTmp = $mWords[$sWord]
			$mTmp = $aTmp[1]
			MapAppend($mTmp, $iPos - StringLen($sWord))
			$aTmp[1] = $mTmp
		Else
			Local $mTmp[]
			Local $aTmp[2] = [$iWord, $mTmp]
		EndIf
		$mWords[$sWord] = $aTmp

		$iWord += 1
	Until 0

	; bereite die notwendigen Replaces auf
	Local $aReplaces[$iWord][3], $iIndRepl = 0
	For $sWord In MapKeys($mWords)
		$iLen = StringLen($sWord)
		$aTmp = $mWords[$sWord]
		$iWord = $aTmp[0]
		$mPos = $aTmp[1]

		If Ubound($mPos) < 1 Then ContinueLoop
		If $iLen <= Stringlen($iWord) Then ContinueLoop

		For $i In MapKeys($mPos)
			$iPos = $mPos[$i]

			$aReplaces[$iIndRepl][0] = $iPos
			$aReplaces[$iIndRepl][1] = $iLen
			$aReplaces[$iIndRepl][2] = $iWord

			$iIndRepl += 1

		Next
	Next
	Redim $aReplaces[$iIndRepl][3]
	_ArraySort($aReplaces)

	; Wörter durch Nummern im Text selbst ersetzen
	For $i = UBound($aReplaces) - 1 To 0 Step -1
		$sString = 	StringLeft($sString, $aReplaces[$i][0] - 1) & _
					$aReplaces[$i][2] & _
					StringTrimLeft($sString, $aReplaces[$i][0] + $aReplaces[$i][1] -1)
	Next

	Return $sString
EndFunc


; Tokenbasierte Dekompression - stellt einen mit _strcmp_compressByWordRepetition() komprimierten String wieder her
Func _strcmp_decompressByWordRepetition($sString)
	Local 	$aTmp[3], _
			$mWords[]
			$iPos = 1

	; Einzelne Wörter aus dem String extrahieren
	Do
		$aMatch = StringRegExp($sString, '(?s)\G.*?(\b[a-zA-ZäöüßÄÖÜ][\wäöüßÄÖÜ]{2,}\b|\b(?<!\x1B)\d+\b)', 1, $iPos)
		IF @error Then ExitLoop
		$iPos = @extended

		$aTmp[0] = $aMatch[0]
		$aTmp[1] = $iPos - StringLen($aMatch[0])
		$aTmp[2] = StringLen($aMatch[0])
		MapAppend($mWords, $aTmp)
	Until 0

	Local $aWords = _map_IntMap2Array($mWords)
	$aWords = _ArrayAinATo2d($aWords)

	; Nummern wieder durch Buchstaben ersetzen
	Local $iWord
	For $i = UBound($aWords) - 1  To 0 Step -1
		If Not StringRegExp($aWords[$i][0], "^\d+$", 0) Then ContinueLoop

		$iWord = Int($aWords[$i][0])
		If $iWord >= UBound($aWords) Then ContinueLoop
		;~ ConsoleWrite($iWord & @CRLF)
		$sString = 	StringLeft($sString, $aWords[$i][1] - 1) & _
					$aWords[$iWord][0] & _
					StringTrimLeft($sString, $aWords[$i][1] + $aWords[$i][2] -1)
	Next

	; Escapte Zahlen wieder unescapen:
	$sString = StringRegExpReplace($sString, '(?s)\x1B(\d+)', "$1")

	Return $sString
EndFunc


; #FUNCTION# ======================================================================================
; Name ..........: _map_IntMap2Array()
; Description ...: returns the values of a map only (useful for array-list like maps produced by MapAppend())
; Syntax ........: _map_IntMap2Array(ByRef $aMap)
; Parameters ....: ByRef $aMap - a map variable
; Return values .: 1D-array containing the values
; Author ........: aspirinjunkie
; Modified ......: 2022-07-13
; =================================================================================================
Func _map_IntMap2Array(ByRef $aMap)
	Local $aRet[UBound($aMap)]
	Local $iInd = 0
	For $vEl In $aMap
		$aRet[$iInd] = $vEl
		$iInd += 1
	Next
	Return $aRet
EndFunc   ;==>_map_IntMap2Array


; #FUNCTION# ======================================================================================
; Name ..........: _ArrayAinATo2d()
; Description ...: Convert a Arrays in Array into a 2D array
; Syntax ........: _ArrayAinATo2d(ByRef $A)
; Parameters ....: $A             - the arrays in array which should be converted
; Return values .: Success: a 2D Array build from the input array
;                  Failure: Null
;                     @error = 1: $A is'nt an 1D array
;                            = 2: $A is empty
;                            = 3: first element isn't a array
; Author ........: AspirinJunkie
; =================================================================================================
Func _ArrayAinATo2d(ByRef $A)
	If UBound($A, 0) <> 1 Then Return SetError(1, UBound($A, 0), Null)
	Local $N = UBound($A)
	If $N < 1 Then Return SetError(2, $N, Null)
	Local $u = UBound($A[0])
	If $u < 1 Then Return SetError(3, $u, Null)

	Local $a_Ret[$N][$u]

	For $i = 0 To $N - 1
		Local $t = $A[$i]
		If UBound($t) > $u Then ReDim $a_Ret[$N][UBound($t)]
		For $j = 0 To UBound($t) - 1
			$a_Ret[$i][$j] = $t[$j]
		Next
	Next
	Return SetExtended($N, $a_Ret)
EndFunc   ;==>_ArrayAinATo2d
```
</details>

which leads to the following result:

```
Original Size: 157081 Bytes
Encoded Size:  73333 Bytes
Compress ratio: 53.3 %
$sString == $sStringDecoded: True
```