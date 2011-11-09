Attribute VB_Name = "Module1"
Sub Macro1()
'
' Macro1 Macro
' ArreglaDBF
'
' Keyboard Shortcut: Ctrl+a
'
    Columns("B:E").Select
    Selection.Insert Shift:=xlToRight
    Columns("G:G").Select
    Selection.Copy
    Columns("B:B").Select
    ActiveSheet.Paste
    Columns("F:F").Select
    Application.CutCopyMode = False
    Selection.Copy
    Columns("C:C").Select
    ActiveSheet.Paste
    Columns("A:A").Select
    Application.CutCopyMode = False
    Selection.Copy
    Application.CutCopyMode = False
    Columns("A:A").Select
    Selection.Copy
    Columns("D:D").Select
    ActiveSheet.Paste
    Columns("E:E").Select
    ActiveSheet.Paste
    Range("A2").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "1"
    Range("A3").Select
    ActiveCell.FormulaR1C1 = "2"
    Range("A2:A3").Select
    Range("A3").Activate
    Selection.AutoFill Destination:=Range("A2:A18864")
    Range("A2:A18864").Select
    Range("A15").Select
    Selection.End(xlDown).Select
    Selection.End(xlUp).Select
    Columns("A:A").Select
    Selection.Copy
    Columns("D:D").Select
    ActiveSheet.Paste
    Columns("E:E").Select
    ActiveSheet.Paste
    Columns("H:H").Select
    Application.CutCopyMode = False
    Selection.Insert Shift:=xlToRight
    ActiveWindow.ScrollColumn = 2
    ActiveWindow.ScrollColumn = 3
    ActiveWindow.ScrollColumn = 4
    ActiveWindow.ScrollColumn = 5
    ActiveWindow.ScrollColumn = 6
    ActiveWindow.ScrollColumn = 7
    ActiveWindow.ScrollColumn = 8
    ActiveWindow.ScrollColumn = 9
    ActiveWindow.ScrollColumn = 10
    ActiveWindow.ScrollColumn = 11
    ActiveWindow.ScrollColumn = 12
    ActiveWindow.ScrollColumn = 13
    ActiveWindow.ScrollColumn = 14
    ActiveWindow.ScrollColumn = 15
    ActiveWindow.ScrollColumn = 16
    ActiveWindow.ScrollColumn = 17
    ActiveWindow.ScrollColumn = 18
    ActiveWindow.ScrollColumn = 19
    ActiveWindow.ScrollColumn = 20
    ActiveWindow.ScrollColumn = 21
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 24
    ActiveWindow.ScrollColumn = 25
    ActiveWindow.ScrollColumn = 26
    ActiveWindow.ScrollColumn = 27
    'Application.Left = 30.25
    'Application.Top = 79
    Columns("AS:AS").Select
    Selection.Copy
    ActiveWindow.ScrollColumn = 26
    ActiveWindow.ScrollColumn = 24
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 19
    ActiveWindow.ScrollColumn = 18
    ActiveWindow.ScrollColumn = 16
    ActiveWindow.ScrollColumn = 14
    ActiveWindow.ScrollColumn = 12
    ActiveWindow.ScrollColumn = 11
    ActiveWindow.ScrollColumn = 9
    ActiveWindow.ScrollColumn = 8
    ActiveWindow.ScrollColumn = 7
    ActiveWindow.ScrollColumn = 6
    ActiveWindow.ScrollColumn = 5
    ActiveWindow.ScrollColumn = 4
    ActiveWindow.ScrollColumn = 3
    ActiveWindow.ScrollColumn = 2
    ActiveWindow.ScrollColumn = 1
    Columns("H:H").Select
    ActiveSheet.Paste
    ActiveWindow.ScrollColumn = 2
    ActiveWindow.ScrollColumn = 3
    ActiveWindow.ScrollColumn = 4
    ActiveWindow.ScrollColumn = 5
    ActiveWindow.ScrollColumn = 6
    ActiveWindow.ScrollColumn = 8
    ActiveWindow.ScrollColumn = 10
    ActiveWindow.ScrollColumn = 12
    ActiveWindow.ScrollColumn = 14
    ActiveWindow.ScrollColumn = 15
    ActiveWindow.ScrollColumn = 16
    ActiveWindow.ScrollColumn = 18
    ActiveWindow.ScrollColumn = 19
    ActiveWindow.ScrollColumn = 21
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 24
    ActiveWindow.ScrollColumn = 25
    ActiveWindow.ScrollColumn = 26
    ActiveWindow.ScrollColumn = 27
    Columns("AS:AS").Select
    Application.CutCopyMode = False
    Selection.Delete Shift:=xlToLeft
    Range("AS2").Select
    ActiveCell.FormulaR1C1 = "=IF(LEFT(R1C[-36],1)=""T"",RC[-36]/10,RC[-36])"
    Range("AS3").Select
    ActiveWindow.SmallScroll ToRight:=18
    Range("AS2").Select
    Selection.AutoFill Destination:=Range("AS2:CB2"), Type:=xlFillDefault
    Range("AS2:CB2").Select
    Selection.AutoFill Destination:=Range("AS2:CB18864")
    Range("AS2:CB18864").Select
    ActiveWindow.ScrollColumn = 62
    ActiveWindow.ScrollColumn = 61
    ActiveWindow.ScrollColumn = 60
    ActiveWindow.ScrollColumn = 59
    ActiveWindow.ScrollColumn = 58
    ActiveWindow.ScrollColumn = 56
    ActiveWindow.ScrollColumn = 54
    ActiveWindow.ScrollColumn = 53
    ActiveWindow.ScrollColumn = 52
    ActiveWindow.ScrollColumn = 51
    ActiveWindow.ScrollColumn = 50
    ActiveWindow.ScrollColumn = 49
    ActiveWindow.ScrollColumn = 48
    ActiveWindow.ScrollColumn = 47
    ActiveWindow.ScrollColumn = 46
    ActiveWindow.ScrollColumn = 45
    ActiveWindow.ScrollColumn = 44
    ActiveWindow.ScrollColumn = 43
    ActiveWindow.ScrollColumn = 42
    ActiveWindow.ScrollColumn = 40
    ActiveWindow.ScrollColumn = 39
    ActiveWindow.ScrollColumn = 38
    ActiveWindow.ScrollColumn = 37
    ActiveWindow.ScrollColumn = 36
    ActiveWindow.ScrollColumn = 35
    ActiveWindow.ScrollColumn = 34
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 32
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 34
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 34
    ActiveWindow.ScrollColumn = 35
    ActiveWindow.ScrollColumn = 36
    ActiveWindow.ScrollColumn = 37
    ActiveWindow.ScrollColumn = 38
    ActiveWindow.ScrollColumn = 39
    ActiveWindow.ScrollColumn = 40
    ActiveWindow.ScrollColumn = 41
    ActiveWindow.ScrollColumn = 42
    ActiveWindow.ScrollColumn = 43
    ActiveWindow.ScrollColumn = 44
    ActiveWindow.ScrollColumn = 43
    ActiveWindow.ScrollColumn = 42
    Columns("AS:CB").Select
    Selection.Copy
    ActiveWindow.ScrollColumn = 64
    ActiveWindow.ScrollColumn = 63
    ActiveWindow.ScrollColumn = 62
    ActiveWindow.ScrollColumn = 60
    ActiveWindow.ScrollColumn = 59
    ActiveWindow.ScrollColumn = 58
    ActiveWindow.ScrollColumn = 57
    ActiveWindow.ScrollColumn = 56
    ActiveWindow.ScrollColumn = 55
    ActiveWindow.ScrollColumn = 54
    ActiveWindow.ScrollColumn = 53
    ActiveWindow.ScrollColumn = 52
    ActiveWindow.ScrollColumn = 51
    ActiveWindow.ScrollColumn = 50
    ActiveWindow.ScrollColumn = 48
    ActiveWindow.ScrollColumn = 46
    ActiveWindow.ScrollColumn = 45
    ActiveWindow.ScrollColumn = 43
    ActiveWindow.ScrollColumn = 41
    ActiveWindow.ScrollColumn = 39
    ActiveWindow.ScrollColumn = 38
    ActiveWindow.ScrollColumn = 36
    ActiveWindow.ScrollColumn = 34
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 31
    ActiveWindow.ScrollColumn = 30
    ActiveWindow.ScrollColumn = 29
    ActiveWindow.ScrollColumn = 28
    ActiveWindow.ScrollColumn = 27
    ActiveWindow.ScrollColumn = 26
    ActiveWindow.ScrollColumn = 25
    ActiveWindow.ScrollColumn = 24
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 21
    ActiveWindow.ScrollColumn = 20
    ActiveWindow.ScrollColumn = 19
    ActiveWindow.ScrollColumn = 18
    ActiveWindow.ScrollColumn = 17
    ActiveWindow.ScrollColumn = 18
    ActiveWindow.ScrollColumn = 19
    ActiveWindow.ScrollColumn = 20
    ActiveWindow.ScrollColumn = 21
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 24
    ActiveWindow.ScrollColumn = 25
    ActiveWindow.ScrollColumn = 26
    ActiveWindow.ScrollColumn = 27
    ActiveWindow.ScrollColumn = 28
    ActiveWindow.ScrollColumn = 29
    ActiveWindow.ScrollColumn = 30
    ActiveWindow.ScrollColumn = 31
    ActiveWindow.ScrollColumn = 32
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 34
    Selection.PasteSpecial Paste:=xlPasteValues, Operation:=xlNone, SkipBlanks _
        :=False, Transpose:=False
    Columns("I:AR").Select
    Range("AR1").Activate
    Application.CutCopyMode = False
    Selection.Delete Shift:=xlToLeft
    Range("I1").Select
    ActiveWindow.ScrollColumn = 4
    ActiveWindow.ScrollColumn = 5
    ActiveWindow.ScrollColumn = 6
    ActiveWindow.ScrollColumn = 7
    ActiveWindow.ScrollColumn = 8
    ActiveWindow.ScrollColumn = 9
    ActiveWindow.ScrollColumn = 10
    ActiveWindow.ScrollColumn = 11
    ActiveWindow.ScrollColumn = 13
    ActiveWindow.ScrollColumn = 14
    ActiveWindow.ScrollColumn = 16
    ActiveWindow.ScrollColumn = 17
    ActiveWindow.ScrollColumn = 18
    ActiveWindow.ScrollColumn = 20
    ActiveWindow.ScrollColumn = 21
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 24
    ActiveWindow.ScrollColumn = 25
    ActiveWindow.ScrollColumn = 26
    ActiveWindow.ScrollColumn = 28
    ActiveWindow.ScrollColumn = 30
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 35
    ActiveWindow.ScrollColumn = 37
    ActiveWindow.ScrollColumn = 40
    ActiveWindow.ScrollColumn = 42
    ActiveWindow.ScrollColumn = 44
    ActiveWindow.ScrollColumn = 46
    ActiveWindow.ScrollColumn = 48
    ActiveWindow.ScrollColumn = 49
    ActiveWindow.ScrollColumn = 50
    ActiveWindow.ScrollColumn = 51
    ActiveWindow.ScrollColumn = 52
    ActiveWindow.ScrollColumn = 53
    ActiveWindow.ScrollColumn = 54
    ActiveWindow.ScrollColumn = 53
    ActiveWindow.ScrollColumn = 52
    ActiveWindow.ScrollColumn = 51
    ActiveWindow.ScrollColumn = 50
    ActiveWindow.ScrollColumn = 49
    ActiveWindow.ScrollColumn = 48
    ActiveWindow.ScrollColumn = 46
    ActiveWindow.ScrollColumn = 45
    ActiveWindow.ScrollColumn = 43
    ActiveWindow.ScrollColumn = 41
    ActiveWindow.ScrollColumn = 39
    ActiveWindow.ScrollColumn = 38
    ActiveWindow.ScrollColumn = 36
    ActiveWindow.ScrollColumn = 35
    ActiveWindow.ScrollColumn = 34
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 32
    ActiveWindow.ScrollColumn = 31
    ActiveWindow.ScrollColumn = 30
    ActiveWindow.ScrollColumn = 29
    ActiveWindow.ScrollColumn = 28
    ActiveWindow.ScrollColumn = 27
    ActiveWindow.ScrollColumn = 26
    ActiveWindow.ScrollColumn = 25
    ActiveWindow.ScrollColumn = 24
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 21
    ActiveWindow.ScrollColumn = 20
    ActiveWindow.ScrollColumn = 19
    ActiveWindow.ScrollColumn = 18
    ActiveWindow.ScrollColumn = 16
    ActiveWindow.ScrollColumn = 15
    ActiveWindow.ScrollColumn = 14
    ActiveWindow.ScrollColumn = 13
    ActiveWindow.ScrollColumn = 12
    ActiveWindow.ScrollColumn = 11
    ActiveWindow.ScrollColumn = 10
    ActiveWindow.ScrollColumn = 9
    ActiveWindow.ScrollColumn = 8
    ActiveWindow.ScrollColumn = 7
    ActiveWindow.ScrollColumn = 6
    ActiveWindow.ScrollColumn = 5
    ActiveWindow.ScrollColumn = 4
    ActiveWindow.ScrollColumn = 3
    ActiveWindow.ScrollColumn = 2
    ActiveWindow.ScrollColumn = 1
    'Application.Left = 220
    'Application.Top = 86.5
    Range("A1").Select
    ActiveCell.FormulaR1C1 = "PONTID"
    Range("B1").Select
    ActiveCell.FormulaR1C1 = "LATITUD"
    Range("C1").Select
    ActiveCell.FormulaR1C1 = "LONGITUD"
    Range("D1").Select
    ActiveCell.FormulaR1C1 = "RecNo"
    Range("E1").Select
    ActiveCell.FormulaR1C1 = "PontNo"
    Range("F1").Select
    ActiveCell.FormulaR1C1 = "Long"
    Range("E1").Select
    ActiveCell.FormulaR1C1 = "PointNo"
    Range("F1").Select
    ActiveCell.FormulaR1C1 = "Long_ext"
    Range("G1").Select
    ActiveCell.FormulaR1C1 = "Lat_ext"
    Range("H1").Select
    ActiveCell.FormulaR1C1 = "alt"
    Range("I1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin_1"
    Range("I1").Select
    ActiveCell.FormulaR1C1 = "tmin1"
    Range("J1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax1"
    Range("K1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec1"
    Range("L1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin2"
    Range("M1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax2"
    Range("N1").Select
    'Application.Left = 43.75
    'Application.Top = 85
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec2"
    Range("O1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin3"
    Range("P1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax3"
    Range("Q1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec3"
    Range("R1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin4"
    Range("S1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax4"
    Range("T1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec4"
    Range("U1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin5"
    Range("V1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax5"
    Range("W1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec5"
    Range("X1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin6"
    Range("Y1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax6"
    Range("Z1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec6"
    Range("AA1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin7"
    Range("AB1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax7"
    Range("AC1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec7"
    Range("AD1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin8"
    Range("AE1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax8"
    Range("AF1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec8"
    Range("AG1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin9"
    Range("AH1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax9"
    Range("AI1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec9"
    Range("AJ1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin10"
    Range("AK1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax10"
    Range("AL1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec10"
    Range("AM1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin11"
    Range("AN1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax11"
    Range("AO1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec11"
    Range("AP1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmin12"
    Range("AQ1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "tmax12"
    Range("AR1").Select
    Selection.NumberFormat = "0"
    ActiveCell.FormulaR1C1 = "prec12"
    Range("AR1").Select
    Selection.End(xlToLeft).Select
    Rows("18864:18864").Select
    Selection.Delete Shift:=xlUp
End Sub
