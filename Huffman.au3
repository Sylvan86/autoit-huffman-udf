;~ #AutoIt3Wrapper_Run_AU3Check=Y
;~ #AutoIt3Wrapper_Au3Check_Parameters=-d -w 1 -w 2 -w 3 -w 4 -w 5 -w 6 -w 7
;~ #AutoIt3Wrapper_AU3Check_Stop_OnWarning=Y
#include-once
#include <Array.au3>
#include <Math.au3>
#include "BinaryHelpers.au3"

; #INDEX# =======================================================================================================================
; Title .........: Huffman-UDF
; Version .......: 0.2
; AutoIt Version : 3.3.16.1
; Language ......: english (german maybe by accident)
; Description ...: Functions to efficiently encode strings using Huffman encoding to compress them.
;                  To keep the overhead for the required Huffman table as low as possible
;                  the canonical form of the Huffman encoding is used.
; Author(s) .....: AspirinJunkie
; Last changed ..: 2023-05-17
; License .......: This work is free.
;                  You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2,
;                  as published by Sam Hocevar.
;                  See http://www.wtfpl.net/ for more details.
; ===============================================================================================================================

; #CURRENT# =====================================================================================================================
; _huff_decodeBinary
; _huff_encodeString
; _huff_decodeBinaryToString
; _huff_decompressCanonicalHuffTable
; _huff_getUniqueCharArray
; _huff_buildCodeTable
; _huff_convert2CanonicalHuffman
; _huff_encodeString2CodeArray
; _huff_compressCanonicalHuffTable
;
; __huff_createLengthArrayCompressed
; __huff_PreSortCanonical
; __huff_Int2BinString
; __huff_IntMap2Array
; __huff_getNextMinPair
; ===============================================================================================================================

; definition of the elements of the huff table
Global Const $_HUFF_TABLE_ELEMENT_SIZE = 4
Global Enum $__HUFF_eHUFFVALUE, $__HUFF_eHUFFCODESTRING, $__HUFF_CODEVALUE, $__HUFF_eCODELENGTH

; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_decodeBinary
; Description ...: decodes a string which is encoded by _huff_encodeString()
; Syntax ........: _huff_decodeBinary(ByRef $bEncoded)
; Parameters ....: $bEncoded            - [in/out] the binary in the structure as returned by _huff_encodeString()
; Return values .: the decoded string
; Author ........: AspirinJunkie
; Modified ......: 2023-05-17
; Related .......: _huff_encodeString
; ===============================================================================================================================
Func _huff_decodeBinary(ByRef $bEncoded)
	Local 	$aHuffTable = _huff_decompressCanonicalHuffTable($bEncoded), _
			$iEndHuffTable = @extended

	; mirror bits of the values
	; Reverse bit direction of the individual values (so that readout works)
	For $i = 0 To UBound($aHuffTable) - 1
		$aHuffTable[$i][$__HUFF_CODEVALUE] = _bin_mirrorBits($aHuffTable[$i][$__HUFF_CODEVALUE], $aHuffTable[$i][$__HUFF_eCODELENGTH])
	Next

	; extract the encoded data from the whole binary
	Local $bCompressedString = BinaryMid($bEncoded, $iEndHuffTable)

	; convert back huff-encoded stream into string
	Return _huff_decodeBinaryToString($bCompressedString, $aHuffTable)

EndFunc

; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_encodeString
; Description ...: Encodes a string via canonical Huffman encoding into a binary which contains all information to decode.
; Syntax ........: _huff_encodeString(ByRef $sString)
; Parameters ....: $sString             - [in/out] the string to be coded
; Return values .: Binary containing the canonical Huffman table information and the encoded string
; Author ........: AspirinJunkie
; Modified ......: 2023-05-17
; Related .......: _huff_decodeBinary (for decoding the result)
; ===============================================================================================================================
Func _huff_encodeString(ByRef $sString)

	; determine the chars and their specific number inside the string
	Local $aSymbols = _huff_getUniqueCharArray($sString)

	; build a normal huff-table out of the symbol list
	Local $aHuffTable = _huff_buildCodeTable($aSymbols)
	;~ _ArrayDisplay($aHuffTable, "Huff-table", "", 64, "|", "char|binary representation|value|bits")

	; convert Huffmann codes to canonical form
	_huff_convert2CanonicalHuffman($aHuffTable)
	;~ _ArrayDisplay($aHuffTable, "Huff-table canonical", "", 64, "|", "char|binary representation|value|bits")

	; Reverse bit direction of the individual values (so that readout works)
	For $i = 0 To UBound($aHuffTable) - 1
		$aHuffTable[$i][$__HUFF_CODEVALUE] = _bin_mirrorBits($aHuffTable[$i][$__HUFF_CODEVALUE], $aHuffTable[$i][$__HUFF_eCODELENGTH])
	Next
	;~ _ArrayDisplay($aHuffTable, "Huff-table sorted", "", 64, "|", "char|binary representation|value|bits")

	; encode every char of the string with it's huffman-code:
	Local $aStringEncodedTable = _huff_encodeString2CodeArray($aHuffTable, $sString)
	;~ _ArrayDisplay($aStringEncodedTable, "encoded chars of string", "", 64, "|", "value|n bits")

	; convert the value array into a bitstream (= huffman encoded representation of the string)
	Local $bCompressedString = _bin_BitArray2Memory($aStringEncodedTable)
	;~ ConsoleWrite(StringLeft($bCompressedString, 20) & @CRLF)

	; compress the canonical huff-table (list of chars and their huffman-codes in canonical form) into a binary
	Local $bHuffCompressed = _huff_compressCanonicalHuffTable($aHuffTable)
	;~ ConsoleWrite($bHuffCompressed & @CRLF)

	; create total compressed data
	Local $tCompressed = DllStructCreate("align 1;Byte Hufftable[" & BinaryLen($bHuffCompressed) & "];Byte StringCompressed[" & BinaryLen($bCompressedString)  & "]")
	DllStructSetData($tCompressed, 1, $bHuffCompressed)
	DllStructSetData($tCompressed, 2, $bCompressedString)

	Return _bin_StructToBin($tCompressed)
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_decodeBinaryToString
; Description ...: decodes a huffman encoded string in binary form using the huff table
; Syntax ........: _huff_decodeBinaryToString(ByRef $bBinary,  ByRef $aHuffTable)
; Parameters ....: $bBinary             - [in/out] The huffman encoded string present as binary
;                  $aHuffTable          - The huffman table as a 2D array as returned by _huff_buildCodeTable() or _huff_decompressCanonicalHuffTable()
; Return values .: the decoded string
; Author ........: AspirinJunkie
; Modified ......: 2023-05-26
; ===============================================================================================================================
Func _huff_decodeBinaryToString(ByRef $bBinary, ByRef $aHuffTable)
	Local 	$nHuffChars = UBound($aHuffTable), _
			$iByteCurr = 0, _
			$iBitPos = 3, _
			$nShift, _
			$nMask, _
			$nValueFull, _
			$nValue, _
			$nCodeLenCurr = 0, _
			$sRet = "", _
			$nLastBit = BinaryLen($bBinary) * 8 - BitAnd(BinaryMid($bBinary, 1, 1), 7) ; last bit

	; work through the binary step by step detecting and extracting the individual characters
	Do
		$iByteCurr = Floor($iBitPos / 8) + 1
		$nShift = - Mod($iBitPos , 8)
		$nCodeLenCurr = 0

		; Byte range in which the current value is contained
		$nValueFull = BinaryMid($bBinary, $iByteCurr, 4)

		; Go through all characters in the huffman-table and if match, then write out characters
		For $iChar = 0 To $nHuffChars - 1

			; extract value for current length once instead of for each value separately.
			If $aHuffTable[$iChar][$__HUFF_eCODELENGTH] <> $nCodeLenCurr Then
				$nCodeLenCurr = $aHuffTable[$iChar][$__HUFF_eCODELENGTH]

				; extract the value at the specific bit startin position with his specific code length
				$nMask = BitShift(2^$nCodeLenCurr - 1, $nShift)
				$nValue = BitShift(BitAND($nValueFull, $nMask), -$nShift)
			EndIf

			; If current value corresponds to current character write to output
			If $nValue = $aHuffTable[$iChar][$__HUFF_CODEVALUE] Then
				$sRet &= $aHuffTable[$iChar][$__HUFF_eHUFFVALUE]
				ExitLoop
			EndIf
		Next

		$iBitPos += $nCodeLenCurr
	Until $iBitPos >= $nLastBit


	Return $sRet
EndFunc



; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_decompressCanonicalHuffTable
; Description ...: Decompress a canonical Huffman table compressed as a binary by _huff_compressCanonicalHuffTable()
; Syntax ........: _huff_decompressCanonicalHuffTable(ByRef $bHuffCompressed)
; Parameters ....: $bHuffCompressed     - [in/out] the binary holding the compressed huffman table
; Return values .: huffman table as 2D-Array: [[character #1, binary representation #1, code value as integer #1, number of bits #1], ..., [...] ]
; Author ........: AspirinJunkie
; Modified ......: 2023-05-09
; ===============================================================================================================================
Func _huff_decompressCanonicalHuffTable(ByRef $bHuffCompressed)
	; The first 2 bytes contain information about the maximum number of bits for the values as well as the maximum number of bits needed to encode the number per length.
	Local $iLenMax = Int(BinaryMid($bHuffCompressed, 1, 1)), _
		  $nBitsMax = Int(BinaryMid($bHuffCompressed, 2, 1))


	; restores the array in which the number of bits of length of the elements in the bitstream is written one after the other.
	Local $aBitsPerElement[$iLenMax], $nBitsForLengths
	For $i = 1 To $iLenMax
		$aBitsPerElement[$i-1] = _Min(2^$i, $nBitsMax)
		$nBitsForLengths += $aBitsPerElement[$i-1]
	Next

	; rebuilds the array from the bitstream with the number of elements per code length
	Local 	$iStartLengths = 3, _
			$iSizeLengths  = Ceiling($nBitsForLengths / 8), _ 		           							; lengths-array: start and size
			$bLengthArea   = BinaryMid($bHuffCompressed, $iStartLengths, $iSizeLengths), _         		; the raw length array binary
			$aElsPerLength = _bin_BitStreamToArray($bLengthArea, $aBitsPerElement), _              		; reconstructed array from binary  , $nBytes4Length = @extended
			$iStartChars   = $iStartLengths + $iSizeLengths, _                                     		; start of chars-area
			$nSizeCharArea = _bin_BinToDataType(BinaryMid($bHuffCompressed, $iStartChars, 2), "USHORT") ; size in bytes for the chars string

	; Determine the total size of the huffman table
	Local $nElements = 0
	For $i In $aElsPerLength
		$nElements += $i
	Next

	; Restore characters
	Local $aChars = StringSplit(BinaryToString(BinaryMid($bHuffCompressed, $iStartChars + 2, $nSizeCharArea), 4), "", 2)

	; reconstruct the values of the canonical Huff table based on the number of respective elements per bit length:
	Local 	$aHuffTable[$nElements][$_HUFF_TABLE_ELEMENT_SIZE], $iVal = 0, $iEl = 0, $iLength = 1

	For $iElements In $aElsPerLength
		For $i = 0 To $iElements - 1
			$iVal += 1

			$aHuffTable[$iEl][$__HUFF_CODEVALUE] = $iVal - 1
			$aHuffTable[$iEl][$__HUFF_eCODELENGTH] = $iLength
			$aHuffTable[$iEl][$__HUFF_eHUFFVALUE] = $aChars[$iEl]
			;~ $aHuffTable[$iEl][$__HUFF_eHUFFCODESTRING] = __huff_Int2BinString($iVal - 1, $iLength)

			$iEl += 1
		Next
		$iVal = BitShift($iVal, -1)
		$iLength += 1
	Next

	; values are still different, because a mirrorbits still has to be done
	Return SetExtended($iStartChars + $nSizeCharArea + 2, $aHuffTable)
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_getUniqueCharArray
; Description ...: creates a sorted frequency table for all occurring characters of a string
; Syntax ........: _huff_getUniqueCharArray(ByRef $sString)
; Parameters ....: $sString             - [in/out] the input string
; Return values .: Success: 2D-Array: [[char #1, frequency #1], [char #2, frequency #2], ..., [char #n, frequency #n]]
;                  Error:   Null and set @error to:
;                           1 : couldn't determine chars
; Author ........: AspirinJunkie
; Modified ......: 2022-03-28
; ===============================================================================================================================
Func _huff_getUniqueCharArray(ByRef $sString)
	Local $mFreq[] ; map holding the chars and their frequency

	; count the frequency for every char in $sString
	For $cChar In StringSplit($sString, "", 2)
		$mFreq[$cChar] += 1
	Next

	; error checking
	If Ubound($mFreq) < 1 Then Return SetError(1, Ubound($mFreq), Null)

	; convert the frequency map into a 2D-Array
	Local $aCharsHist[Ubound($mFreq)][2]
	Local $i = 0
	For $sChar In MapKeys($mFreq)
		$aCharsHist[$i][0] = $sChar
		$aCharsHist[$i][1] = $mFreq[$sChar]
		$i += 1
	Next

	; sort descending by frequency
	_ArraySort($aCharsHist, 1, 0, 0, 1)

	Return $aCharsHist
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_buildCodeTable
; Description ...: generates a Huffman coding from a frequency table
; Syntax ........: _huff_buildCodeTable(ByRef $aSymbols)
; Parameters ....: $aSymbols            - [in/out] char freqency table as returned by _huff_getUniqueCharArray()
; Return values .: huffman table as 2D-Array: [[character #1, binary representation #1, code value as integer #1, number of bits #1], ..., [...] ]
; Author ........: AspirinJunkie
; Modified ......: 2022-03-28
; Related .......: __huff_getNextMinPair()
; ===============================================================================================================================
Func _huff_buildCodeTable(ByRef $aSymbols)
	Local Enum $eFreq, $eLevel, $eParInd        ; structure of a node in the huffman-tree (count, tree level, parent index)

	; fill the first level of the Hufman tree
	Local $mNodes[] ; the tree
	Local $aElTemp[3] = [0, 0, 0] ; a empty tree node
	For $i = 0 To UBound($aSymbols) - 1
		$aElTemp[$eFreq] = $aSymbols[$i][1]
		$aElTemp[$eLevel] = 0

		MapAppend($mNodes, $aElTemp)
	Next

	; build the Huffman tree
	Local $iA, $iB, $aA, $aB, $iP, $aPair
	Do
		; determine the 2 smallest elements in the tree that do not yet have a parent
		$aPair = __huff_getNextMinPair($mNodes)

		; only consider free nodes
		If $aPair[3] < 0 Then ExitLoop

		; index of the left node and right node
		$iA = $aPair[2]
		$iB = $aPair[3]

		; the left node and right node
		$aA = $mNodes[$iA]
		$aB = $mNodes[$iB]

		; create new parent node
		Local $aElTemp[3] = [$aA[$eFreq] + $aB[$eFreq], _Max($aA[$eLevel], $aB[$eLevel]) + 1, 0]
		$iP = MapAppend($mNodes, $aElTemp)
		$aA[$eParInd] = $iP
		$aB[$eParInd] = -$iP    ; right branch - encoded by negation

		$mNodes[$iA] = $aA
		$mNodes[$iB] = $aB
	Until 0

	; create the code table out of the huffman tree
	Local $aTable[UBound($aSymbols)][$_HUFF_TABLE_ELEMENT_SIZE]
	Local $aNode, $iValTmp = 0
	For $i = 0 To UBound($aSymbols) - 1
		$aTable[$i][$__HUFF_eHUFFVALUE] = $aSymbols[$i][0]
		$aTable[$i][$__HUFF_CODEVALUE] = 0 ; special case if a element has 0 as value - otherwise it would result in a null string

		$aNode = $mNodes[$i]
		$iValTmp = 1
		Do
			If $aNode[$eParInd] = 0 Then ExitLoop
			If $aNode[$eParInd] > 1 Then
				$aTable[$i][$__HUFF_eHUFFCODESTRING] = "0" & $aTable[$i][1]
			Else
				$aTable[$i][$__HUFF_eHUFFCODESTRING] = "1" & $aTable[$i][1]
				$aTable[$i][$__HUFF_CODEVALUE] += $iValTmp
			EndIf

			$aNode = $mNodes[Abs($aNode[$eParInd])]
			$iValTmp += $iValTmp ; *= 2
		Until 0

		$aTable[$i][$__HUFF_eCODELENGTH] = StringLen($aTable[$i][1])
	Next

	Return $aTable
EndFunc   ;==>_huff_buildCodeTable


; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_convert2CanonicalHuffman
; Description ...: convert a normal huffman table into their canonical form
; Syntax ........: _huff_convert2CanonicalHuffman(ByRef $aHuffTable[,  Const $nMaxBitSize = 16])
; Parameters ....: $aHuffTable          - [in/out] a huffman table as returned by _huff_buildCodeTable()
;                  $nMaxBitSize         - [optional] maximum length of the huffman codes (default value should be o.k. for many cases)
; Return values .: 1D array holding the number of elements for every code length
; Author ........: AspirinJunkie
; Modified ......: 2022-03-28
; Remarks .......: Instead of listing and saving all Huffman codes individually,
;                  you can simply calculate the codes if you know the number of each code length.
;                  In this way, one only needs to save the code length frequencies to save space and can recover the Huffman codes with this alone.
;                  This kind of representation is called the "canonical form of the Huffman table".
; Related .......: _huff_buildCodeTable(), __huff_PreSortCanonical()
; ===============================================================================================================================
Func _huff_convert2CanonicalHuffman(ByRef $aHuffTable, Const $nMaxBitSize = 20)
	; sort by length and lexically
	__huff_PreSortCanonical($aHuffTable)

	Local $aLengths[$nMaxBitSize]	; number of elements per code length

	$aHuffTable[0][$__HUFF_eHUFFCODESTRING] = StringRegExpReplace($aHuffTable[0][$__HUFF_eHUFFCODESTRING], ".", "0")
	$aHuffTable[0][$__HUFF_CODEVALUE] = 0
	$aLengths[$aHuffTable[0][$__HUFF_eCODELENGTH]] = 1	; increase number for first code length by 1

	; convert current Huffman table into canonical form
	Local 	$iVal = 0, _
			$iLengthBefore = $aHuffTable[0][$__HUFF_eCODELENGTH], _	; code length of the previous element
			$iLengthCurrent
	For $i = 1 To UBound($aHuffTable) - 1
		$iVal += 1
		$iLengthCurrent = $aHuffTable[$i][$__HUFF_eCODELENGTH]

		If $iLengthCurrent <> $iLengthBefore Then
			$iVal = BitShift($iVal, $iLengthBefore - $iLengthCurrent)
			$iLengthBefore = $iLengthCurrent
		EndIf

		$aHuffTable[$i][$__HUFF_CODEVALUE] = $iVal
		$aHuffTable[$i][$__HUFF_eHUFFCODESTRING] = __huff_Int2BinString($iVal, $iLengthCurrent)

		$aLengths[$iLengthCurrent] += 1
	Next

	Return $aLengths
EndFunc   ;==>_huff_createCanonicalHuffman


; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_encodeString2CodeArray
; Description ...: converts a string to a huffmann-encoded code array using a huff table
; Syntax ........: _huff_encodeString2CodeArray(ByRef $aHuffTable,  ByRef $sString)
; Parameters ....: $aHuffTable          - [in/out] a huff table as returned by _huff_buildCodeTable or modified by _huff_convert2CanonicalHuffman()
;                  sString              - the string which should be encoded
; Return values .: 2D-Array with encoded char-values: [[value #1, number of bits #1], [value #2, number of bits #2], ..., [value #n, number of bits #n]]
; Author ........: AspirinJunkie
; Modified ......: 2022-05-26
; Related .......: _huff_buildCodeTable(), _huff_convert2CanonicalHuffman()
; ===============================================================================================================================
Func _huff_encodeString2CodeArray(ByRef $aHuffTable, ByRef $sString)
	Local 	$nChars = StringLen($sString), _
			$nHuffChars = UBound($aHuffTable), _
			$iPos = 1, _
			$sSubCurr = "", _
			$nLenCurr, _
			$aBitStream[$nChars+1][2] = [[0,3]], _
			$iEl = 1, _
			$iBitCurr = 3 ; count the current bit position

	; convert Huff table to map for faster searching:
	Local $bOnlySingleChars = True, $mHuff[]
	For $i = 0 To $nHuffChars - 1
		Local $aHuffEl[2] = [$aHuffTable[$i][$__HUFF_CODEVALUE], $aHuffTable[$i][$__HUFF_eCODELENGTH]]
		$mHuff[$aHuffTable[$i][$__HUFF_eHUFFVALUE]] = $aHuffEl
		If StringLen($aHuffTable[$i][$__HUFF_eHUFFVALUE]) > 1 Then $bOnlySingleChars = False
	Next


	If $bOnlySingleChars Then ; faster when only single chars are elements

		For $cChar In StringSplit($sString, "", 3)

			; get huff element for current substring:
			$aHuffEl = $mHuff[$cChar]
			If Not IsArray($aHuffEl) Then Return SetError(1,0,Null)

			; write encoded value to output array
			$aBitStream[$iEl][0] = $aHuffEl[0]
			$aBitStream[$iEl][1] = $aHuffEl[1]

			$iEl += 1
			$iBitCurr += $aHuffEl[1]
		Next

	Else
		Do
			$nLenCurr = StringLen($aHuffTable[0][$__HUFF_eHUFFVALUE])
			$sSubCurr = StringMid($sString, $iPos, $nLenCurr)

			For $i = 0 To $nHuffChars - 1
				If StringLen($aHuffTable[$i][$__HUFF_eHUFFVALUE]) <> $nLenCurr Then
					$nLenCurr = StringLen($aHuffTable[$i][$__HUFF_eHUFFVALUE])
					$sSubCurr = StringMid($sString, $iPos, $nLenCurr)
				EndIf

				If $aHuffTable[$i][$__HUFF_eHUFFVALUE] == $sSubCurr Then ; substring matches current element
					$aBitStream[$iEl][0] = $aHuffTable[$i][$__HUFF_CODEVALUE]
					$aBitStream[$iEl][1] = $aHuffTable[$i][$__HUFF_eCODELENGTH]

					$iPos += $nLenCurr
					$iEl += 1
					$iBitCurr += $aBitStream[$iEl][1]
					ExitLoop
				EndIf
			Next
		Until $iPos > $nChars
		Redim $aBitStream[$iEl][UBound($aBitStream,2)]

	EndIf

	; add the number of unused bits (at the end of the stream) to the first 3 bits of the bit stream
	Local $nRest = Mod($iBitCurr, 8)
	If $nRest = 0 Then $nRest = 8
	$aBitStream[0][0] = 8 - $nRest

	Return $aBitStream
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _huff_compressCanonicalHuffTable
; Description ...: Takes a canonical huff table and stores it as compactly as possible as a binary
; Syntax ........: _huff_compressCanonicalHuffTable(ByRef $aHuffTable)
; Parameters ....: $aHuffTable          - [in/out] a 2D-Array canonical huffman table as returned by _huff_convert2CanonicalHuffman()
; Return values .: a binary holding the compressed canonical huffman table
; Author ........: AspirinJunkie
; Modified ......: 2023-05-09
; ===============================================================================================================================
Func _huff_compressCanonicalHuffTable(ByRef $aHuffTable)
	Local $bLengthArray = __huff_createLengthArrayCompressed($aHuffTable)

	; Add the elements themselves
	Local $sChars = ""
	For $i = 0 To UBound($aHuffTable) - 1
		$sChars &= $aHuffTable[$i][$__HUFF_eHUFFVALUE]
	Next

	Local $bChars = StringToBinary($sChars, 4)

	; write all elements one after the other into memory
	Local $tHuffCanonic = DllStructCreate("align 1;Byte Lengths[" & BinaryLen($bLengthArray) & "]; USHORT nSizeChars; Byte Chars[" & BinaryLen($bChars) & "]")

	DllStructSetData($tHuffCanonic, 1, $bLengthArray)
	DllStructSetData($tHuffCanonic, 2, BinaryLen($bChars))
	DllStructSetData($tHuffCanonic, 3, $bChars)

	Return _bin_StructToBin($tHuffCanonic)
EndFunc



; #INTERNAL_USE_ONLY# ===========================================================================================================
; Name ..........: __huff_Int2BinString
; Description ...: converts an integer to a binary string (chars 0/1) with a fixed length
; Syntax ........: __huff_Int2BinString(Const $iVal[,  Const $nDigits = 8])
; Parameters ....: $iVal                - the integer value
;                  $nDigits             - [optional] number of digits to be printed
; Return values .: string with chars 0/1 holding the binary representation of the value
; Author ........: AspirinJunkie
; Modified ......: 2023-05-09
; ===============================================================================================================================
Func __huff_Int2BinString(Const $iVal, Const $nDigits = 8)
	Local 	$iDigitVal = 1, _
			$sRet = ""
	For $i = 1 To $nDigits
		$sRet = (BitAND($iVal, $iDigitVal) ? "1" : "0") & $sRet
		$iDigitVal += $iDigitVal
	Next
	Return $sRet
EndFunc


; #INTERNAL_USE_ONLY# ===========================================================================================================
; Name ..........: __huff_IntMap2Array
; Description ...: converts the elements of an integer map into a 1D array
; Syntax ........: __huff_IntMap2Array(ByRef $aMap)
; Parameters ....: $aMap                - [in/out] a integer map as build with MapAppend()
; Return values .: 1D-Array with the elements
; Author ........: AspirinJunkie
; Modified ......: 2023-05-09
; Remarks .......: keys of a integer map go without gaps from 0 upwards. If there are gaps - it will fail
; ===============================================================================================================================
Func __huff_IntMap2Array(ByRef $aMap)
	Local $aRet[UBound($aMap)]
	Local $iInd = 0
	For $vEl In $aMap
		$aRet[$iInd] = $vEl
		$iInd += 1
	Next
	Return $aRet
EndFunc



; #INTERNAL_USE_ONLY# ===========================================================================================================
; Name ..........: __huff_getNextMinPair
; Description ...: auxiliary function to determine the next 2 free(!) nodes, which have the lowest rank, during the creation of the huff tree.
; Syntax ........: __huff_getNextMinPair(ByRef $mLevels)
; Parameters ....: $mLevels             - [in/out] the huffman tree as used in _huff_buildCodeTable
; Return values .: Array[6]: [1st value, 2nd value, 1st index, 2nd index, 1st level, 2nd level]
; Author ........: AspirinJunkie
; Modified ......: 2022-03-28
; Related .......: _huff_buildCodeTable()
; ===============================================================================================================================
Func __huff_getNextMinPair(ByRef $mLevels)
	Local Enum $eFreq, $eLevel, $eParLevel, $eParInd        ; structure of a node in the huffman-tree (count, tree level, parent index)
	Local Enum $eFirstVal, $eSecondVal, $eFirstIndex, $eSecondIndex, $eFirstLevel, $eSecondLevel ; structure of the return array (node values of two nodes)
	Local $aA
	Local $aRet[6] = [2 ^ 63, 2 ^ 63, -1, -1]

	; loop over all nodes and determine the next 2 free nodes
	For $i = 0 To UBound($mLevels) - 1
		$aA = $mLevels[$i]
		If $aA[$eParLevel] <> 0 Then ContinueLoop ; only consider free nodes

		If $aA[$eFreq] < $aRet[$eFirstVal] Or ($aA[$eFreq] = $aRet[$eFirstVal] And $aA[$eLevel] < $aRet[$eFirstLevel]) Then
			$aRet[$eSecondVal] = $aRet[$eFirstVal]
			$aRet[$eSecondIndex] = $aRet[$eFirstIndex]
			$aRet[$eSecondLevel] = $aRet[$eFirstLevel]

			$aRet[$eFirstVal] = $aA[$eFreq]
			$aRet[$eFirstIndex] = $i
			$aRet[$eFirstLevel] = $aA[$eLevel]

		ElseIf $i <> $aRet[$eFirstIndex] And (($aA[$eFreq] < $aRet[$eSecondVal]) Or ($aA[$eFreq] = $aRet[$eSecondVal] And $aA[$eLevel] < $aRet[$eSecondLevel])) Then
			$aRet[$eSecondVal] = $aA[$eFreq]
			$aRet[$eSecondIndex] = $i
			$aRet[$eSecondLevel] = $aA[$eLevel]
		EndIf
	Next

	Return $aRet
EndFunc   ;==>__huff_getNextMinPair

; #INTERNAL_USE_ONLY# ===========================================================================================================
; Name ..........: __huff_PreSortCanonical
; Description ...: Prepare a Huffman table for conversion to canonical form.
;                  For this purpose, the table is sorted 1. by code length and 2. lexically.
; Syntax ........: __huff_PreSortCanonical(ByRef $aHuffTable)
; Parameters ....: $aHuffTable          - [in/out] the huffman table as returned by _huff_buildCodeTable()
; Return values .: none (huffman table is sorted in-place)
; Author ........: AspirinJunkie
; Modified ......: 2022-03-28
; Remarks .......: sorting algorithm used: Bucket-Sort where the code length define the buckets and insertion sort for sort inside the buckets
; Related .......: _huff_buildCodeTable()
; ===============================================================================================================================
Func __huff_PreSortCanonical(ByRef $aHuffTable)
	Local $mLengthBuckets[], _     ; bucket container for the lengths
			$aElement[$_HUFF_TABLE_ELEMENT_SIZE], _
			$nCodeLength, _
			$nMax = 1

	; create the buckets and sort the elements into their corresponding bucket
	For $i = 0 To UBound($aHuffTable) - 1
		$nCodeLength = $aHuffTable[$i][$__HUFF_eCODELENGTH]

		If Not MapExists($mLengthBuckets, $nCodeLength) Then ; Initialize map for bucket
			Local $mTmp[]
			$mLengthBuckets[$nCodeLength] = $mTmp
		EndIf

		$aElement[$__HUFF_eHUFFVALUE] = $aHuffTable[$i][$__HUFF_eHUFFVALUE]
		$aElement[$__HUFF_eHUFFCODESTRING] = $aHuffTable[$i][$__HUFF_eHUFFCODESTRING]
		$aElement[$__HUFF_CODEVALUE] = $aHuffTable[$i][$__HUFF_CODEVALUE]
		$aElement[$__HUFF_eCODELENGTH] = $nCodeLength
		MapAppend($mLengthBuckets[$nCodeLength], $aElement)

		If $nMax < $nCodeLength Then $nMax = $nCodeLength
	Next

	; sort inside the buckets
	Local $nBucketSize, $aBucket, $t1, $t2, $j, $iC = 0, $mLengthBucket
	For $i = 1 To $nMax
		$mLengthBucket = $mLengthBuckets[$i]
		If Not IsMap($mLengthBucket) Then ContinueLoop
		$aBucket = __huff_IntMap2Array($mLengthBucket)
		$nBucketSize = UBound($aBucket)

		For $iA = 0 To $nBucketSize - 1	; insertion sort of bucket
			$t1 = $aBucket[$iA]
				For $j = $iA - 1 To 0 Step -1
				$t2 = $aBucket[$j]
				If StringCompare($t1[$__HUFF_eHUFFCODESTRING], $t2[$__HUFF_eHUFFCODESTRING]) <> -1 Then ExitLoop
				$aBucket[$j + 1] = $t2
			Next
			$aBucket[$j + 1] = $t1
		Next

		; overwrite old huffman-table
		For $iB = 0 To $nBucketSize - 1
			$aElement = $aBucket[$iB]
			$aHuffTable[$iC][$__HUFF_eHUFFVALUE] = $aElement[$__HUFF_eHUFFVALUE]
			$aHuffTable[$iC][$__HUFF_eHUFFCODESTRING] = $aElement[$__HUFF_eHUFFCODESTRING]
			$aHuffTable[$iC][$__HUFF_CODEVALUE] = $aElement[$__HUFF_CODEVALUE]
			$aHuffTable[$iC][$__HUFF_eCODELENGTH] = $aElement[$__HUFF_eCODELENGTH]
			$iC += 1
		Next
	Next
EndFunc   ;==>__huff_PreSortCanonical


; #INTERNAL_USE_ONLY# ===========================================================================================================
; Name ..........: __huff_createLengthArrayCompressed
; Description ...: Compresses the length array needed for the canonical representation of the Huffman table (number of elements for each bit length).
; Syntax ........: __huff_createLengthArrayCompressed(ByRef $aHuffTable)
; Parameters ....: $aHuffTable          - [in/out] a 2D-Array canonical huffman table as returned by _huff_convert2CanonicalHuffman()
; Return values .: a binary holding the compressed length array
; Author ........: AspirinJunkie
; Modified ......: 2023-05-09
; ===============================================================================================================================
Func __huff_createLengthArrayCompressed(ByRef $aHuffTable)
	Local $nElems = UBound($aHuffTable), _
		  $aLengths[($nElems < 10 ? 10 : $nElems)][2], _
		  $iLenCurr = 0, _
		  $iLenMax = 0, _  ; max code length
		  $iCodesMax = 0   ; max count of codes for length

	; count the number of variant codes for every code length and determine the max code length
	For $i = 0 To Ubound($aHuffTable) - 1
		$iLenCurr = $aHuffTable[$i][$__HUFF_eCODELENGTH]
		$aLengths[$iLenCurr-1][0] += 1

		; determine max length count
		If $iCodesMax < $aLengths[$iLenCurr-1][0] Then $iCodesMax = $aLengths[$iLenCurr-1][0]

		; determine max code length
		If $iLenMax < $iLenCurr Then $iLenMax = $iLenCurr
	Next
	Redim $aLengths[$iLenMax][2]

	; determine the maximum needed bits to encode the length value
	Local $nBitsMax = Floor(log($iCodesMax)/log(2)) + 1

	; write the length array
	For $i = 1 To $iLenMax
		$aLengths[$i-1][1] = _Min(2^($i), $nBitsMax)
	Next

	; convert the lengths to binary stream
	Local $bLengths = _bin_BitArray2Memory($aLengths)

	; old version (only 1 byte for params but can lead to range overflows)
	;~ DllStructSetData($tHuffCanonic, 1, _
	;~ 	BitAND($iLenMax - 2, 15) + _   ; -2 Da 0 oder 1 maximale Länge sinnlos sind - auf die Art kann in 4 Bits bis 17 abgebildet werden 
	;~ 	BitShift(BitAnd($nBitsMax - 1, 7), -4) _ ;  -1, da 2^0 = 1 in der Praxis unwahrscheinlich ist. Somit Wertebereich von 1-8 anstatt 0-7	
	;~ )
	Local $tHuffCanonic = DllStructCreate("align 1;BYTE iLenMax; BYTE nBitsMax; Byte Lengths[" & BinaryLen($bLengths) & "]")
	DllStructSetData($tHuffCanonic, 1, $iLenMax)
	DllStructSetData($tHuffCanonic, 2, $nBitsMax)
	DllStructSetData($tHuffCanonic, 3, $bLengths)

	Return _bin_StructToBin($tHuffCanonic)
EndFunc