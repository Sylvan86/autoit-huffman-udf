;~ #AutoIt3Wrapper_Run_AU3Check=Y
;~ #AutoIt3Wrapper_Au3Check_Parameters=-d -w 1 -w 2 -w 3 -w 4 -w 5 -w 6 -w 7
;~ #AutoIt3Wrapper_AU3Check_Stop_OnWarning=Y
#include-once

; #INDEX# =======================================================================================================================
; Title .........: BinaryHelpers
; Version .......: 0.3
; AutoIt Version : 3.3.16.1
; Language ......: english (german maybe by accident)
; Description ...: Functions which help to handle binaries on byte and bit level.
;                  Primary focus on techniques to map data as space efficient as possible.
; Author(s) .....: AspirinJunkie
; Modified ......: 2023-02-07
; License .......: This work is free.
;                  You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2,
;                  as published by Sam Hocevar.
;                  See http://www.wtfpl.net/ for more details.
; ===============================================================================================================================

; #VARIABLES# ===================================================================================================================
Global $__iCODINGDEFAULT = 4 ; global parameter to set the default encoding like in StringToBinary()
; ===============================================================================================================================

; #CONSTANTS# ===================================================================================================================
Global CONST $__fBINHELP_LOG2 = Log(2) ; constant for log(2) to prevent re-calculation
; ===============================================================================================================================

; #CURRENT# =====================================================================================================================
; _bin_StructToBin              - create a binary variable out of a AutoIt-DllStruct
; _bin_BinToStruct              - converts a binary variable to a DLLStruct with the corresponding structure
; _bin_BinToDataType            - interprets a binary variable as a DllStruct data type
; _bin_swapEndianess            - swaps the endianess of a binary
; _bin_mirrorBits               - mirrors the used(!) bits (not bytes) of a number
; _bin_StringArrayTo0Terminated - converts a StringArray into a single binary in which the strings are 0-terminated
; _bin_0TerminatedToStringArray - converts a binary containing 0-terminated strings into an array with these substrings
; _bin_BitArray2Memory	        - writes list of values with variable bit length in sequence into memory
; _bin_BitStreamToArray         - reads consecutive single values from a binary, but their lengths can be different
; _bin_binaryShrink             - converts an array of positive integers into a binary in which the numbers are stored sequentially with a specific number of bits per element
; _bin_binaryUnshrink           - interprete a binary as if it were an array with element size $iSize (bits)
; _bin_shrinkString2Bin         - converts a string to a binary and shrinks it, if it can be represented with less than 8 bits per character.
; _bin_unshrinkString2Bin       - decompress a string encoded by _bin_shrinkString2Bin()
; _bin_Int2BinString            - converts an integer to a binary string ("0"/"1") with a fixed length
; ===============================================================================================================================


; #FUNCTION# ======================================================================================
; Name ..........: _bin_StructToBin
; Description ...: create a binary variable out of a AutoIt-DllStruct
; Syntax ........: _bin_StructToBin(ByRef $tStruct)
; Parameters ....: $tStruct    - DllStruct you want the binary expression from
; Return values .: Success     - Return a binary with the dllstruct-content as data
; Author ........: AspirinJunkie
; Modified ......: 2023-02-07
; =================================================================================================
Func _bin_StructToBin(ByRef $tStruct)
	Local $tReturn = DllStructCreate("Byte[" & DllStructGetSize($tStruct) & "]", DllStructGetPtr($tStruct))
	Return DllStructGetData($tReturn, 1)
EndFunc

; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_BinToStruct
; Description ...: converts a binary variable to a DLLStruct with the corresponding structure
; Syntax ........: _bin_BinToStruct($bBinary,  $tagStruct)
; Parameters ....: $bBinary             - the binary variable which should be converted
;                  $tagStruct           - the structure string as in DllStructCreate
; Return values .: a DllStruct variable
; Author ........: AspirinJunkie
; Modified ......: 2023-05-17
; ===============================================================================================================================
Func _bin_BinToStruct($bBinary, $tagStruct)
	; create a full size struct and fill it with raw binary
	Local $nSize = BinaryLen($bBinary)
	Local $tTmp = DllStructCreate("BYTE[" & $nSize & "]")
	If @error Then Return SetError(1, @error, NULL)
	DllStructSetData($tTmp, 1, $bBinary)
	If @error Then Return SetError(2, @error, NULL)

	; interprete the written data in a special form
	Local $tRet = DllStructCreate($tagStruct, DllStructGetPtr($tTmp))
	If @error Then Return SetError(3, @error, NULL)

	Return $tRet
EndFunc

; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_BinToDataType
; Description ...: interprets a binary variable as a DllStruct data type
; Syntax ........: _bin_BinToDataType($bBin,  $sType)
; Parameters ....: $bBin       - the binary which contains the data
;                  $sType      - struct/datatype definition as in DllStructCreate()
; Return values .: Success     - if only a single datatype (no ";" inside $sType) is given: single value
;                                if multiple datatypes are defined: array with values
;                  Failure      - Return Null and set @error to:
;        				@error = 1 - error during DllStructCreate (see @extended for the specific @error)
;                              = 2 - error during DllStructSetData (see @extended for the specific @error)
;                              = 3 - error during _bin_BinToStruct (see @extended for the specific @error)
;                              = 4 - error during DllStructGetData (see @extended for the specific @error)
; Author ........: AspirinJunkie
; Modified ......: 2023-05-17
; Related .......: _bin_BinToStruct
; ===============================================================================================================================
Func _bin_BinToDataType($bBin, $sType)

	; multiple values (return array)
	If StringInStr($sType, ';', 1) Then
		StringReplace($sType, ';', ';', 0, 1)
		Local $nElements = @extended + 1
		Local $aRet[$nElements]

		; convert binary into a dllstruct variable
		Local $tTmp = _bin_BinToStruct($bBin, $sType)
		If @error Then Return SetError(3, @error, Null)

		For $i = 1 To $nElements
; TODO: if elements are array then add a corresponding query

			$aRet[$i-1] = DllStructGetData($tTmp, $i)
			Switch @error
				Case 0
					ContinueLoop
				Case 2
					Redim $aRet[$i - 1]
					ExitLoop ; can occure when "align" is used
				Case Else
					Return SetError(4, @error, Null)
			EndSwitch
		Next

		Return $aRet

	; single value (return scalar)
	Else

		Local $tTemp = DllStructCreate($sType)
		If @error Then Return SetError(1, @extended, Null)

		DllStructSetData($tTemp, 1, $bBin)
		If @error Then Return SetError(2, @extended, Null)

		Return DllStructGetData($tTemp, 1)

	EndIf
EndFunc

; #FUNCTION# ======================================================================================
; Name ..........: _bin_swapEndianess
; Description ...: swaps the endianess of a binary (little-endian to big-endian and vice-versa)
; Syntax ........: _bin_swapEndianess($dBig)
; Parameters ....: $dBig       - binary which should be converted
; Return values .: Success     - Return a binary with swapped endianess
;                  Failure      - Return Null and set @error to:
;        				@error = 1 - error during calling ntohs
;                              = 2 - error during calling ntohl
;                              = 3 - error during calling ntohl processing the higher bytes of 64 Bit-data
;                              = 4 - error during calling ntohl processing the lower bytes of 64 Bit-data
; Author ........: AspirinJunkie
; Modified ......: 2023-02-07
; =================================================================================================
Func _bin_swapEndianess($dBig)
    Local Static $hDll = DllOpen("ws2_32.dll")

    Switch BinaryLen($dBig)
		; use win api functions if possible
        Case 2
            Local $aRet = DllCall($hDll, "USHORT", "ntohs", "USHORT", $dBig)
            Return @error ? SetError(1, @error, Null) : BinaryMid($aRet[0], 1, 2)            ;  BinaryMid(Binary(BitAnd(BitShift($dBig, 8), 255) + BitShift(BitAnd($dBig, 255), -8)), 1, 2)
        Case 4
            Local $aRet = DllCall($hDll, "ULONG", "ntohl", "ULONG", $dBig)
            Return @error ? SetError(2, @error, Null) : Number($aRet[0], 1)
        Case 8
            Local $aRetHigh = DllCall($hDll, "ULONG", "ntohl", "ULONG", BinaryMid($dBig, 1, 4))
            IF @error Then Return SetError(3, @error, Null)

            Local $aRetLow = DllCall($hDll, "ULONG", "ntohl", "ULONG", BinaryMid($dBig, 5, 4))
            IF @error Then Return SetError(4, @error, Null)

            Local $t64Bit = DllStructCreate("ULONG;ULONG")
            DllStructSetData($t64Bit, 1, $aRetLow[0])
            DllStructSetData($t64Bit, 2, $aRetHigh[0])

            Local $tReturn = DllStructCreate("UINT64", DllStructGetPtr($t64Bit))
            Return DllStructGetData($tReturn, 1)

		; if not then do it manually
        Case Else
            Local $iLen = BinaryLen($dBig)

            Local $tBytes = DllStructCreate("BYTE[" & $iLen & "]")
            Local $iTargetPos = $iLen, $i
            For $i = 1 To $iLen
                DllStructSetData($tBytes, 1, BinaryMid($dBig, $i, 1), $iTargetPos)
                $iTargetPos -= 1
            Next
            Return DllStructGetData($tBytes, 1)
    EndSwitch
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_BitArray2Memory
; Description ...: writes a sequence of values with variable bit length in sequence into memory
; Syntax ........: _bin_BitArray2Memory(ByRef $aValues)
; Parameters ....: $aValues             - [in/out] 2D input array: [[Value 1, NumberOfBits 1], [Value 2, NumberOfBits 2], ...]
; Return values .: Binary variable holding the bit stream
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; ===============================================================================================================================
Func _bin_BitArray2Memory(ByRef $aValues)
	Local 	$nBitsWhole = 0, _
			$nElements = UBound($aValues), _
			$iBitPos = 0, _
			$iByteCurr = 0, _
			$nShift = 0, _
			$tTmpInt, _
			$nValOld, _
			$nValNew

	; determine required memory
	For $i = 0 To $nElements - 1
		$nBitsWhole += $aValues[$i][1]
	Next
	Local 	$nBytes = Ceiling($nBitsWhole / 8), _
			$tNewBinByte = DllStructCreate("BYTE [" & $nBytes + 8 & "]"), _ ; Int-values are putted byte by byte - so buffer of 8 to prevent a range overflow
			$pStart = DllStructGetPtr($tNewBinByte, 1)

	For $i = 0 To $nElements - 1
		$iByteCurr = Floor($iBitPos / 8)
		$nShift = - Mod($iBitPos , 8)

		$tTmpInt = DllStructCreate("INT", $pStart + $iByteCurr)
		$nValOld = DllStructGetData($tTmpInt, 1)
		$nValNew = $nValOld + BitShift($aValues[$i][0], $nShift)
		DllStructSetData($tTmpInt, 1, $nValNew)

		$iBitPos += $aValues[$i][1]
	Next

	Return BinaryMid(DllStructGetData($tNewBinByte, 1), 1, $nBytes )
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_BitStreamToArray
; Description ...: Reads consecutive single values from a binary, but their lengths can be different
;                  (Inverse function of _bin_BitArray2Memory)
; Syntax ........: _bin_BitStreamToArray(ByRef $bBinary,  ByRef $aLengths)
; Parameters ....: $bBinary             - [in/out] the binary sequence
;                  $aLengths            - Array with the length sequence for the individual elements
; Return values .: 1D array with the extracted element values
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; ===============================================================================================================================
Func _bin_BitStreamToArray(ByRef $bBinary, ByRef $aLengths)
	Local 	$nLen, _
			$iByteCurr = 0, _
			$iBitPos = 0, _
			$nShift, _
			$nMask, _
			$nValueFull, _
			$aRet[UBound($aLengths)], _
			$iEl = 0

	For $nLen In $aLengths
		$iByteCurr = Floor($iBitPos / 8) + 1
		$nShift = - Mod($iBitPos , 8)

		$nValueFull = BinaryMid($bBinary, $iByteCurr, 8)
		$nMask = BitShift(2^$nLen - 1, $nShift)
		$aRet[$iEl] = BitShift(BitAND($nValueFull, $nMask), -$nShift)

		$iBitPos += $nLen
		$iEl += 1
	Next

	Return SetExtended($iByteCurr, $aRet)
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_StringArrayTo0Terminated
; Description ...: converts a StringArray into a binary in which the strings are 0-terminated
; Syntax ........: _bin_StringArrayTo0Terminated(ByRef $aStrings[,  Const $iCoding = $__iCODINGDEFAULT])
; Parameters ....: $aStrings            - [in/out] 1D-array of strings
;                  $iCoding             - [optional] the encoding flag as in StringToBinary
; Return values .: binary with all strings delimited by 00
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; ===============================================================================================================================
Func _bin_StringArrayTo0Terminated(ByRef $aStrings, Const $iCoding = $__iCODINGDEFAULT)
	Local $sTmp = "", $sSubString

	For $sSubString in $aStrings
		$sTmp &= $sSubString & Chr(0)
	Next
	Return StringToBinary(StringTrimRight($sTmp, 1), $iCoding)	; trim because no Chr(0) at the end is needed
EndFunc

; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_0TerminatedToStringArray
; Description ...: converts a binary containing 0-terminated strings into an array with these substrings
; Syntax ........: _bin_0TerminatedToStringArray(ByRef $bStrings[,  Const $iCoding = $__iCODINGDEFAULT])
; Parameters ....: $bStrings            - [in/out] binary with multiple strings delimited by 00
;                  $iCoding             - [optional] the encoding flag as in StringToBinary
; Return values .: 1D-array with extracted substrings
; Aut.......: AspirinJunkie
; Mod ......: 2022-03-31
; ==========================================================================================================================
Func _bin_0TerminatedToStringArray(ByRef $bStrings, Const $iCoding = $__iCODINGDEFAULT)
	Local $nLength = BinaryLen($bStrings)

	Local $sTmp = (BinaryMid($bStrings, $nLength, 1) == Chr(0)) _
				? BinaryToString($bStrings, $iCoding) _
				: BinaryToString(BinaryMid($bStrings, 1, $nLength - 1), $iCoding)

	Return StringSplit($sTmp, CHR(0), 3)
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_mirrorBits
; Description ...: Mirrors the used(!) bits (not bytes) of a number
; Syntax ........: _bin_mirrorBits($iX[,  $nBits = Ceiling(Log($iX + 1) / $__fBINHELP_LOG2)])
; Parameters ....: $iX                  - value where the used bits should get mirrored
;                  $nBits               - [optional] number of bits which should get mirrored
;                                         Default: determine the number of used bits out of the value (necessary bits to represent the value)
; Return values .: the value with mirrored bits
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; ===============================================================================================================================
Func _bin_mirrorBits($iX, $nBits = Ceiling(Log($iX + 1) / $__fBINHELP_LOG2))
	Local $iY = 0
	For $i = 0 To $nBits - 1
		$iY += BitAnd(2^$i, $iX) ? 2^($nBits-$i - 1) : 0
	Next
	Return $iY
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_binaryShrink
; Description ...: Converts an array of positive integers into a binary in which the numbers are stored sequentially.
;                  For each number only $iBitsNew bits of memory are used. The function is thus primarily used to compress number arrays.
; Syntax ........: _bin_binaryShrink(ByRef $aVals,  Const $iBitsNew)
; Parameters ....: $aVals               - [in/out] array of (unsigned) integer values to be compressed
;                  iBitsNew             - number of bits for every value (depends on the largest number in the array: Ceiling(Log($Number + 1) / Log(2)) )
; Return values .: Binary with values written in sequence, each with a size of $iBitsNew
;                  and set @extended to: size in bytes of the returned binary
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; Remarks .......: If values are used which are not unsigned integers or can be interpreted as such or the value range
;                  can not be represented by the number of bits in $iBitsNew, then incorrect results are produced.
;                  The potential may be increased in particular cases with the use of an offset.
;                  E.g. numbers in the range 100-103 can be represented with 2 bits each only if an offset of 100 is applied.
; Related .......: _bin_binaryUnshrink()
; Example .......: #include <Array.au3>
;                  ; compress 8 small numbers into 2 Bytes
;                  Local $aTest[] = [0,1,2,3,3,2,1,0]
;                  $bTest = _bin_binaryShrink($aTest, 2)
;                  ConsoleWrite($bTest & @CRLF)
;                  ; uncompress the values
;                  $aUnshrinked = _bin_binaryUnshrink($bTest, 2)
;                  _ArrayDisplay($aUnshrinked, "Size: " & BinaryLen($bTest) & " Bytes")
; ===============================================================================================================================
Func _bin_binaryShrink(ByRef $aVals, Const $iBitsNew)
	Local $iN = UBound($aVals)

	Local $indBit = 0, _
		$indStartByte, _
		$dValOld, $dValOldL, _
		$dValNew, $dValNewL, $dValNewH, _
		$iShift, _
		$iNewSize = Ceiling($iBitsNew / 8 * $iN), _
		$tNewBinByte = DllStructCreate("BYTE [" & $iNewSize & "]")

	For $indArr = 0 To $iN -1 ; loop through each value in the input array
		$indStartByte = Floor($indBit / 8) ; calculate the index of the start byte for the current value
		$dValOldL = DllStructGetData($tNewBinByte, 1, $indStartByte + 1) ; get the old low byte value
		$dValOld = $dValOldL ; set the old value to the old low byte value

		$iShift = - Mod($indBit + 8, $indStartByte * 8 + 8) ; calculate the shift amount based on the new bit size

		$dValNew = BitShift($aVals[$indArr], $iShift) ; shift the bits of the current value to fit the new bit position
		$dValNewL = BitAND($dValNew, 255) ; get the new low byte value
		$dValNewH = BitShift(BitAND($dValNew, 65280), 8) ; get the new high byte value

		DllStructSetData($tNewBinByte, 1, $dValOldL + $dValNew, $indStartByte + 1) ; set the low byte of the output binary string
		DllStructSetData($tNewBinByte, 1, $dValNewH, $indStartByte + 2) ; set the high byte of the output binary string

		$indBit += $iBitsNew ; increment the bit index by the new bit size
	Next

	Return SetExtended($iNewSize, DllStructGetData($tNewBinByte, 1))
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_binaryUnshrink
; Description ...: interprete a binary as if it were an array with element size $iSize (bits)
; Syntax ........: _bin_binaryUnshrink(ByRef $bCompressed,  $iSize)
; Parameters ....: $bCompressed         - [in/out] Binary with consecutively written values of $iSize bits each
;                  $iSize               - number of bits for every value
; Return values .: array with decoded values
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; Remarks .......: the extracted data can be larger than the original data, because it is rounded up to full bytes.
;                  In this case the function would return more values than actually coded.
;                  To solve this, the encoding must either be given information about how many elements are contained
;                  or information about how many remaining bits should be ignored.
; Related .......: _bin_binaryShrink()
; Example .......: see example of _bin_binaryShrink()
; ===============================================================================================================================
Func _bin_binaryUnshrink(ByRef $bCompressed, $iSize)
	Local $iLengthNew = BinaryLen($bCompressed) * 8 / $iSize, _		; Element count of the decompressed result array (could be more due to rounding up to full bytes)
		$aReturn[$iLengthNew], _					; return array for the decompressed data
		$iShift, _			; the respective offset within
		$indStartByte		; StartByte containing the data of the respective element (+offset still)

	Local Const $iMask = 2^($iSize) -1 ; bitmask to extract the relevant parts

	For $iByteNew = 0 To $iLengthNew - 1
		$indStartByte = Floor($iByteNew * $iSize / 8) ; the byte index where the current value starts

		; shift within the byte for the current value
		$iShift = Mod($iByteNew * $iSize + 8, $indStartByte * 8 + 8) ; could be optimized by replacing by summation in the loop

		; extract the current value, convert it to normal integer and add it to the return array
		$aReturn[$iByteNew] = BitShift(BitAnd(BinaryMid($bCompressed, $indStartByte + 1, 2), Bitshift($iMask, -$iShift)), $iShift)
	Next

	Return $aReturn
EndFunc

; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_shrinkString2Bin
; Description ...: Converts a string to a binary and shrinks it if it can be represented with less than 8 bits per character.
; Syntax ........: _bin_shrinkString2Bin(ByRef $sString)
; Parameters ....: $sString             - [in/out] The string which should be shrunk
; Return values .: A binary with the shrunken string contents.
;                  Structure: 1st byte: 3 bits for number of bits per character and 5 bits for offset in powers of 2.
;                  After that comes the compressed string itself.
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; Remarks .......: the extracted data can be larger than the original data, because it is rounded up to full bytes. (see _bin_binaryShrink() )
;                  In this cases it's recommended to also save the number of chars to split up to this.
; Related .......: _bin_unshrinkString2Bin()
; Example .......: $bShrinked = _bin_shrinkString2Bin('ABBAABBAABBAABBA')
;                  ConsoleWrite("compressed: " & $bShrinked & " (" & BinaryLen($bShrinked) & " Bytes)" & @CRLF)
;                  $sUnshrinked = _bin_unshrinkString2Bin($bShrinked)
;                  ConsoleWrite("decompressed: " & $sUnshrinked & " (" & StringLen($sUnshrinked) & " chars)" & @CRLF)
; ===============================================================================================================================
Func _bin_shrinkString2Bin(ByRef $sString)
	Local $iMax = 0, $iMin = 2^63
	Local $aVals = StringToASCIIArray($sString)

	For $iVal in $aVals
		If $iVal > $iMax Then $iMax = $iVal
		If $iVal < $iMin Then $iMin = $iVal
	Next

	Local $nShiftBits = Floor(log($iMin) / log (2))
	Local $nShift = 2^$nShiftBits + 1
	$iMax -= $nShift
	Local $nBits  = Ceiling(log($iMax) / log (2))
	If $nBits < 1 Then $nBits = 1

	For $i = 0 To UBound($aVals) -1
		$aVals[$i] -= $nShift
	Next
	Local $bShrinked = _bin_binaryShrink($aVals, $nBits)
	Local $nBytes = BinaryLen($bShrinked)

	; first Byte: encode the number of bits/element in the first 3 Bits and bits offset for the values (5 Bits)
	Local $tCompressed = DllStructCreate("align 1;BYTE nBits; BYTE sCompressed[" & $nBytes & "]")
	DllStructSetData($tCompressed, 1, $nBits + BitShift($nShiftBits, -4))
	DllStructSetData($tCompressed, 2, $bShrinked)

	Local $tFull = DllStructCreate("BYTE Full[" & $nBytes + 1 & "]", DllStructGetPtr($tCompressed))
	Return DllStructGetData($tFull, 1)
EndFunc


; #FUNCTION# ====================================================================================================================
; Name ..........: _bin_unshrinkString2Bin
; Description ...: decompress a string encoded by _bin_shrinkString2Bin().
; Syntax ........: _bin_unshrinkString2Bin(ByRef $bShrinkedString)
; Parameters ....: $bShrinkedString     - [in/out] binary as returned by _bin_shrinkString2Bin(). 
; Return values .: the decoded string
; Author ........: AspirinJunkie
; Modified ......: 2022-03-31
; Related .......: _bin_shrinkString2Bin()
; Example .......: see example of _bin_shrinkString2Bin()
; ===============================================================================================================================
Func _bin_unshrinkString2Bin(ByRef $bShrinkedString)
	Local $nBitsRaw = Int(BinaryMid($bShrinkedString, 1, 1))
	Local $nBits = BitAND($nBitsRaw, 15)
	Local $nShiftBits = BitShift(BitAND($nBitsRaw, 240), 4)
	Local $nShift = 2^$nShiftBits + 1

	Local $bTmp = BinaryMid($bShrinkedString, 2, BinaryLen($bShrinkedString) - 1)
	Local $aVals = _bin_binaryUnshrink($bTmp, $nBits)

	For $i = 0 To UBound($aVals) -1
		$aVals[$i] += $nShift
	Next

	Return StringFromASCIIArray($aVals)
EndFunc


; #INTERNAL_USE_ONLY# ===========================================================================================================
; Name ..........: _bin_Int2BinString
; Description ...: converts an integer to a binary string ("0"/"1") with a fixed length
; Syntax ........: _bin_Int2BinString(Const $iVal[,  Const $nDigits = 8])
; Parameters ....: $iVal                - the value
;                  $nDigits             - [optional] number of digits which should be returned
; Return values .: string with the binary representation of $iVal
; Author ........: AspirinJunkie
; ===============================================================================================================================
Func _bin_Int2BinString(Const $iVal, Const $nDigits = 8)
	Local 	$iDigitVal = 1, _
			$sRet = ""
	For $i = 1 To $nDigits
		$sRet = (BitAND($iVal, $iDigitVal) ? "1" : "0") & $sRet
		$iDigitVal += $iDigitVal
	Next
	Return $sRet
EndFunc