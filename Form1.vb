Public Class Form1

    Public u, v, z As Integer 'wobei "u" und "v" jeweils die row und column number des Feldes darstellen, z die Eingabe

    Public xeins As Integer
    Public xdrei As Integer
    Public yeins As Integer
    Public ydrei As Integer

    Dim quadrant As Integer
    Dim timercount As Integer = 10
    Dim var(8, 8, 8) As Integer
    Dim var2(8, 8, 8) As String
    Dim Speicher(8, 8) As Integer
    Dim a, b, c As Integer
    Dim unmög As Integer
    Dim mög As Integer
    Dim gesetzt As Integer
    Dim zahl As Integer
    Dim Save2or3 As Integer = 0
    Dim count As Integer = 0
    Public zähler As Integer
    Public Nur1Leer As Boolean = False

    Private Sub btnclear_Click(sender As System.Object, e As System.EventArgs) Handles btnclear.Click
        For x As Integer = 1 To 9
            For y As Integer = 1 To 9
                For z As Integer = 1 To 9
                    Me.GetControlByName("txt" + CStr(x) + CStr(y)).Text = ""
                    var(x - 1, y - 1, z - 1) = 0
                Next
            Next
        Next
    End Sub

    Public Function GetControlByName(ByVal Name As String) As Control
        Dim info As System.Reflection.FieldInfo = Me.GetType().GetField("_" & Name, System.Reflection.BindingFlags.NonPublic Or System.Reflection.BindingFlags.Instance Or System.Reflection.BindingFlags.Public Or System.Reflection.BindingFlags.IgnoreCase)
        If info Is Nothing Then
            Return Nothing
        End If
        Dim o As Object = info.GetValue(Me)
        Return o
    End Function
    Private Sub btnsolve_Click(sender As System.Object, e As System.EventArgs) Handles btnsolve.Click
        If AllFilled() = True Or SudokuIsEmpty() = True Then
            Exit Sub
        End If

        btnsavethis.PerformClick()
        'Um wiederherstellen zu können

        'Eigentlicher Lösungsweg:

        Dim count As Integer = 0
        Do While count <= 100 ' Beliebig lange iterieren
            UpdateArray()
            SummeVonZ()
            OnePossibility()
            count += 1
        Loop


        If AllFilled() = False Then ' Wenn nicht gelöst werden konnte
            Dim Result1 As DialogResult = MessageBox.Show("Sorry, no solution found." & vbCrLf & "Should I try the long solving-method? This can take up to 30 Seconds.", "Sudoku seems to be tricky", MessageBoxButtons.YesNo)
            If Result1 = MsgBoxResult.No Then
                'nothing
            ElseIf Result1 = MsgBoxResult.Yes Then
                btnReset.PerformClick()
                CandidateLines() ' Dauert 30 Sekundnen (!), aber es kann hilfreich sein.
                Application.DoEvents()
                Do While count <= 50 ' Beliebig lange iterieren
                    UpdateArray()
                    SummeVonZ()
                    OnePossibility()
                    count += 1
                Loop
            End If
        End If
    End Sub

    Private Sub When_Keypress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txt11.KeyPress, txt12.KeyPress, txt13.KeyPress, txt14.KeyPress, txt15.KeyPress, txt16.KeyPress, txt17.KeyPress, txt18.KeyPress, txt19.KeyPress, txt21.KeyPress, txt22.KeyPress, txt23.KeyPress, txt24.KeyPress, txt25.KeyPress, txt26.KeyPress, txt27.KeyPress, txt28.KeyPress, txt29.KeyPress, txt31.KeyPress, txt32.KeyPress, txt33.KeyPress, txt34.KeyPress, txt35.KeyPress, txt36.KeyPress, txt37.KeyPress, txt38.KeyPress, txt39.KeyPress, txt41.KeyPress, txt42.KeyPress, txt43.KeyPress, txt44.KeyPress, txt45.KeyPress, txt46.KeyPress, txt47.KeyPress, txt48.KeyPress, txt49.KeyPress, txt51.KeyPress, txt52.KeyPress, txt53.KeyPress, txt54.KeyPress, txt55.KeyPress, txt56.KeyPress, txt57.KeyPress, txt58.KeyPress, txt59.KeyPress, txt61.KeyPress, txt62.KeyPress, txt63.KeyPress, txt64.KeyPress, txt65.KeyPress, txt66.KeyPress, txt67.KeyPress, txt68.KeyPress, txt69.KeyPress, txt71.KeyPress, txt72.KeyPress, txt73.KeyPress, txt74.KeyPress, txt75.KeyPress, txt76.KeyPress, txt77.KeyPress, txt78.KeyPress, txt79.KeyPress, txt81.KeyPress, txt82.KeyPress, txt83.KeyPress, txt84.KeyPress, txt85.KeyPress, txt86.KeyPress, txt87.KeyPress, txt88.KeyPress, txt89.KeyPress, txt91.KeyPress, txt92.KeyPress, txt93.KeyPress, txt94.KeyPress, txt95.KeyPress, txt96.KeyPress, txt97.KeyPress, txt98.KeyPress, txt99.KeyPress
        'Enable only numbers between 1 to 9 and backspace buttons to be entered in textboxes
        If Asc(e.KeyChar) <> 8 AndAlso Not IsNumeric(e.KeyChar) Or e.KeyChar = "0" Then
            e.Handled = True
        End If
    End Sub
  
    Sub OnePossibility()
        UpdateArray()

        Dim success As Integer = 0
        Dim faillure As Integer = 0
        Dim zahl As Integer = 0
        var(2, 7, 3) = 0
        For u = 0 To 8
            For v = 0 To 8
                For Me.z = 0 To 8
                    If var(u, v, z) <> 10 Then
                        If var(u, v, z) = 1 Then
                            success += 1
                            zahl = Me.z
                        End If
                        If var(u, v, z) = 0 Then
                            faillure += 1
                        End If
                    End If
                Next
                If success = 1 AndAlso faillure = 8 Then
                    ' Nur genau eine Zahl Möglich
                    fill(u + 1, v + 1, zahl + 1)
                    For g = 0 To 8
                        var(u, v, g) = 10
                    Next
                End If

                success = 0
                faillure = 0
                zahl = 0
            Next
        Next

    End Sub
    
   

    Private Sub btnSumofZ_Click(sender As System.Object, e As System.EventArgs) Handles btnSumofZ.Click
        SummeVonZ()
    End Sub
    Sub SummeVonZ()
        Dim sum As Integer
        Dim zeile As Integer
        Dim spalte As Integer
        Dim zahl As Integer


        For Me.z = 0 To 8
            For Me.u = 0 To 8
                zeile = 0
                spalte = 0
                zahl = 0
                sum = 0


                For Me.v = 0 To 8 ' zeilen von links nach rechts
                    If var(u, v, z) <> 10 Then
                        If var(u, v, z) = 1 Then
                            zeile = Me.u
                            spalte = Me.v
                            zahl = Me.z
                        End If
                        sum += var(u, v, z)

                    End If
                    If var(u, v, z) = 10 Then
                        sum += 1
                    End If

                Next
                'Ende von Zeile erreicht
                If sum = 1 Then
                    'MessageBox.Show("zeile + 1, spalte + 1, zahl + 1" & zeile + 1 & "  " & spalte + 1 & "  " & zahl + 1)
                    fill(zeile + 1, spalte + 1, zahl + 1)
                    For g = 0 To 8
                        var(zeile, spalte, g) = 10
                    Next
                End If

          
                zeile = 0
                spalte = 0
                zahl = 0
                sum = 0
            Next

        Next
        UpdateArray()
        For Me.z = 0 To 8
            For Me.v = 0 To 8
                zeile = 0
                spalte = 0
                zahl = 0
                sum = 0

                For Me.u = 0 To 8 ' spalten von oben nach unten
                    If var(u, v, z) <> 10 Then
                        If var(u, v, z) = 1 Then
                            zeile = Me.u
                            spalte = Me.v
                            zahl = Me.z
                        End If
                        sum += var(u, v, z)
                    End If
                Next

                If sum = 1 Then
                    'MessageBox.Show("zeile + 1, spalte + 1, zahl + 1" & zeile + 1 & "  " & spalte + 1 & "  " & zahl + 1)
                    fill(zeile + 1, spalte + 1, zahl + 1)
                    For g = 0 To 8
                        var(zeile, spalte, g) = 10
                    Next
                End If
                zeile = 0
                spalte = 0
                zahl = 0
                sum = 0
            Next
        Next
        UpdateArray()
        For Me.z = 0 To 8
            For q = 1 To 9
                quadrant = q
                GetFieldByQuadrant(quadrant)

                sum = 0
                spalte = 0
                zeile = 0
                zahl = 0



                For x = xeins To xdrei
                    For y = yeins To ydrei
                        ' loop = von links nach rechts, dann nächste Zeile, dasselbe (3x)
                        If var(x - 1, y - 1, z) <> 10 Then
                            If var(x - 1, y - 1, z) = 1 Then
                                zeile = x - 1
                                spalte = y - 1
                                zahl = z
                            End If
                            sum += var(x - 1, y - 1, z)
                        End If
                    Next
                Next

                If sum = 1 Then

                    fill(zeile + 1, spalte + 1, zahl + 1)
                    For g = 0 To 8
                        var(zeile, spalte, g) = 10
                    Next
                End If
                sum = 0
                spalte = 0
                zeile = 0
                zahl = 0
            Next
            zeile = 0
            spalte = 0
            zahl = 0
            sum = 0
        Next
    End Sub
    Private Function IsPossibleContingency(ByVal i As Integer, ByVal contingency As Integer, ByVal Zeile As Integer) As Boolean
        UpdateArray()
        GetFieldByQuadrant(i)
        Dim count As Integer = 0

        Dim x As Integer = Zeile
        For y = yeins To ydrei
            If var(Zeile - 1, y - 1, contingency) = 1 Then
                count += 1
            End If
        Next

        If count = 0 Then
            Return False
        Else
            Return True
        End If
    End Function

    Private Function GetQuadrantByField(ByVal u2 As Integer, ByVal v2 As Integer) As Integer
        '  MessageBox.Show(u2 & " " & v2)

        If u2 < 4 AndAlso v2 < 4 Then
            quadrant = 1
        End If

        If v2 >= 4 Then
            If v2 < 7 Then
                If u2 < 4 Then
                    quadrant = 2
                End If
            End If
        End If

        If v2 >= 7 Then
            If u2 < 4 Then
                quadrant = 3
            End If
        End If

        If v2 <= 3 Then
            If u2 <= 6 Then
                If u2 > 3 Then
                    quadrant = 4
                End If
            End If
        End If

        If v2 <= 6 Then
            If v2 >= 4 Then
                If u2 >= 4 Then
                    If u2 <= 6 Then
                        quadrant = 5
                    End If
                End If
            End If
        End If

        If v2 >= 7 Then
            If u2 >= 4 Then
                If u2 <= 6 Then
                    quadrant = 6
                End If
            End If
        End If

        If v2 <= 3 Then
            If u2 >= 7 Then
                quadrant = 7
            End If
        End If

        If v2 >= 4 Then
            If v2 <= 6 Then
                If u2 >= 7 Then
                    quadrant = 8
                End If
            End If

        End If

        If v2 >= 7 Then
            If u2 >= 7 Then
                quadrant = 9
            End If
        End If

        Return quadrant
    End Function

    Public Sub CandidateLines()
        For contingency As Integer = 0 To 8
            UpdateArray()
            For q = 1 To 9


                If GreenLight(q, contingency) = 3 Or GreenLight(q, contingency) = 2 Then
                    ' wenn nicht 2 oder 3 Möglichkeiten hat, brint der Versuch nichts, deshalb früh genug abschneiden, um Zeit zu sparen
                    UpdateArray()
                    GetFieldByQuadrant(q)

                    Dim zeileU As Integer = 0
                    Dim candidate As Integer = 0
                    Dim sum As Integer = 0

                    If Save2or3 = 2 Then


                        For x = xeins To xdrei
                            For y = yeins To ydrei


                                If var(x - 1, y - 1, contingency) = 1 Then
                                    sum += var(x - 1, y - 1, contingency)
                                    'zwischenspeicher
                                    zeileU = x - 1
                                    candidate = contingency
                                End If
                            Next

                            If sum = 2 Then

                                ' jetzt können Möglichkeiten gelöscht werden, weil man weis, wo diese Zahlen NICHT sein dürfen

                                'MessageBox.Show(" zeileU+1: " & zeileU + 1 & vbCrLf & "quadrant" & q & vbCrLf & "sum: " & sum & "save2or3: " & Save2or3 & "contingency + 1 = " & contingency + 1 & vbCrLf & "Angaben dazu: " & "x: (x ist welche Zeilen Nr " & x & vbCrLf & "Auf 0 gesetzt wird alles auf der Zeile: " & zeileU & " wenn es nicht" & yeins & " oder " & yeins + 1 & " oder " & ydrei & " ist: ")
                                For g As Integer = 1 To 9
                                    GetQuadrantByField(zeileU + 1, g)
                                    ' MessageBox.Show("startquadrant: " & q & vbCrLf & "g: " & g & vbCrLf & "quadrant: " & quadrant)
                                    If quadrant <> q Then
                                        var(zeileU, g - 1, candidate) = 0
                                        var2(zeileU, g - 1, candidate) = "Geändert"

                                    End If

                                Next
                            End If

                            sum = 0
                            zeileU = 0

                        Next

                        Dim spalteV As Integer = 0
                        candidate = 0
                        sum = 0
                        For y = yeins To ydrei
                            For x = xeins To xdrei

                                If var(x - 1, y - 1, contingency) = 1 Then
                                    sum += var(x - 1, y - 1, contingency)
                                    'zwischenspeicher
                                    spalteV = y - 1
                                    candidate = contingency
                                End If
                            Next

                            If sum = 2 Then
                                'Spalte bekannt.
                                'candidate bekannt

                                'MessageBox.Show(" SpalteV+1: " & spalteV + 1 & vbCrLf & "quadrant" & q & vbCrLf & "sum: " & sum & vbCrLf & "save2or3: " & Save2or3 & vbCrLf & " contingency + 1 = " & contingency + 1 & vbCrLf & "Angaben dazu: " & "y: (y ist welche Spalte Nr) " & y & vbCrLf & "Auf 0 gesetzt wird alles auf der Zeile: " & zeileU & " wenn es nicht" & xeins & " oder " & xeins + 1 & " oder " & xeins & " ist: ")
                                For g As Integer = 1 To 9
                                    GetQuadrantByField(g, spalteV + 1)
                                    'MessageBox.Show("startquadrant: " & q & vbCrLf & "g: " & g & vbCrLf & "quadrant: " & quadrant)
                                    If quadrant <> q Then
                                        var(g - 1, spalteV, candidate) = 0
                                        var2(g - 1, spalteV, candidate) = "Geändert"

                                    End If

                                Next

                            End If

                            sum = 0
                            spalteV = 0
                        Next

                    End If
                    'In seltenen Fällen könnten auch drei in einer Reihe sein:
                    If Save2or3 = 3 Then
                        For x = xeins To xdrei
                            For y = yeins To ydrei


                                If var(x - 1, y - 1, contingency) = 1 Then
                                    sum += var(x - 1, y - 1, contingency)
                                    'zwischenspeicher
                                    zeileU = x - 1
                                    candidate = contingency
                                End If
                            Next

                            If sum = 3 Then
                                'Zeile bekannt.
                                'candidate bekannt
                                ' jetzt können Möglichkeiten gelöscht werden, weil man weis, wo diese Zahlen NICHT sein dürfen

                                'MessageBox.Show(" zeileU+1: " & zeileU + 1 & vbCrLf & "quadrant" & q & vbCrLf & "sum: " & sum & "save2or3: " & Save2or3 & "contingency + 1 = " & contingency + 1 & vbCrLf & "Angaben dazu: " & "x: (x ist welche Zeilen Nr " & x & vbCrLf & "Auf 0 gesetzt wird alles auf der Zeile: " & zeileU & " wenn es nicht" & yeins & " oder " & yeins + 1 & " oder " & ydrei & " ist: ")
                                For g As Integer = 1 To 9
                                    GetQuadrantByField(zeileU + 1, g)
                                    ' MessageBox.Show("startquadrant: " & q & vbCrLf & "g: " & g & vbCrLf & "quadrant: " & quadrant)
                                    If quadrant <> q Then
                                        var(zeileU, g - 1, candidate) = 0
                                        var2(zeileU, g - 1, candidate) = "Geändert"
                                    End If

                                Next
                            End If

                            sum = 0
                            zeileU = 0

                        Next
                        'gleicher Vorgang, aber vertikal
                        Dim spalteV As Integer = 0
                        candidate = 0
                        sum = 0
                        For y = yeins To ydrei
                            For x = xeins To xdrei

                                If var(x - 1, y - 1, contingency) = 1 Then
                                    sum += var(x - 1, y - 1, contingency)
                                    'zwischenspeicher
                                    spalteV = y - 1
                                    candidate = contingency
                                End If
                            Next

                            If sum = 3 Then
                                'Spalte bekannt.
                                'candidate bekannt

                                'MessageBox.Show(" SpalteV+1: " & spalteV + 1 & vbCrLf & "quadrant" & q & vbCrLf & "sum: " & sum & vbCrLf & "save2or3: " & Save2or3 & vbCrLf & " contingency + 1 = " & contingency + 1 & vbCrLf & "Angaben dazu: " & "y: (y ist welche Spalte Nr) " & y & vbCrLf & "Auf 0 gesetzt wird alles auf der Zeile: " & zeileU & " wenn es nicht" & xeins & " oder " & xeins + 1 & " oder " & xeins & " ist: ")
                                For g As Integer = 1 To 9
                                    GetQuadrantByField(g, spalteV + 1)
                                    'MessageBox.Show("startquadrant: " & q & vbCrLf & "g: " & g & vbCrLf & "quadrant: " & quadrant)
                                    If quadrant <> q Then
                                        var(g - 1, spalteV, candidate) = 0
                                        var2(g - 1, spalteV, candidate) = "Geändert"
                                    End If

                                Next

                            End If

                            sum = 0
                            spalteV = 0
                        Next

                    End If
                End If
            Next

        Next
    End Sub

    Private Function GreenLight(ByVal quadrant As Integer, ByVal contingency As Integer) As Integer

        Dim g As Integer = 0
        Dim j As Integer = 0
        Dim h As Integer = 0

        GetFieldByQuadrant(quadrant)

        For g = Me.xeins To Me.xdrei
            For j = Me.yeins To Me.ydrei
                UpdateArray()
                If var(g - 1, j - 1, contingency) <> 10 Then
                    h += var(g - 1, j - 1, contingency)
                End If
            Next
        Next
        'MessageBox.Show("h: " & h)
        If h = 2 Or h = 3 Then
            Save2or3 = h
        Else
            Save2or3 = 0
        End If
        Return h


    End Function


    Function checkrow(ByVal z As Integer) As Boolean
        For i = 1 To 9  'jedes textfeld kontrollieren nach z
            If i = u Then
                Continue For
            End If
            If Me.GetControlByName("txt" + CStr(i) + CStr(v)).Text = z.ToString Then
                Return False 'Zahl gibt es schon in dieser Reihe

            End If
        Next
        Return True 'Zahl gibt es noch nicht in dieser Reihe
    End Function
    Function checkcolum(ByVal z As Integer) As Boolean
        For i = 1 To 9  'jedes textfeld kontrollieren nach z
            If i = v Then
                Continue For
            End If
            If Me.GetControlByName("txt" + CStr(u) + CStr(i)).Text = z.ToString Then
                Return False 'Zahl gibt es schon in dieser Spalte

            End If
        Next
        Return True 'Zahl gibt es noch nicht in dieser Spalte
    End Function
    Public Function checkbox(ByVal z As Integer) As Boolean
        getbox(u, v)
        For x = xeins To xdrei
            For y = yeins To ydrei
                If x = u And y = v Then
                    Continue For 'Das eigentliche Feld muss nicht kontrolliert werden
                End If
                If Me.GetControlByName("txt" + CStr(x) + CStr(y)).Text = z.ToString Then
                    Return False 'Zahl gibt es schon in diesem 3x3 Feld
                End If
            Next
        Next
        'Zahl gibt es noch nicht in diesem 3x3 Feld, weil sie nie = z gewesen ist
        Return True
    End Function

    Private Sub Form1_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        For Each c As Control In TableLayoutPanel1.Controls
            If TypeOf c Is TextBox Then
                c.Height = 43
                c.Width = 43

                Dim TBox As TextBox = CType(c, TextBox)
                AddHandler TBox.TextChanged, AddressOf TextBoxTextChanged

            End If
        Next
        '
    End Sub
    Private Sub TextBoxTextChanged(ByVal sender As Object, ByVal e As System.EventArgs)
        ''Der Wert einer TextBox wurde geändert
        'Dim name As String = DirectCast(sender, TextBox).Name

        'Dim valname As String = ""
        'For Each value As Char In name
        '    If value >= "1" And value <= "9" Then
        '        valname &= value
        '    End If
        'Next

        ''u = valname.Substring(0, 1)
        '' v = valname.Substring(1, 1)
        'Dim zwischenspeicherU As Integer = u
        'Dim zwischenspeicherV As Integer = v

        'DirectCast(sender, TextBox).ForeColor = Color.Red

        'u = zwischenspeicherU
        'v = zwischenspeicherV
    End Sub
    Public Sub GetFieldByQuadrant(ByVal quadrant)
        If quadrant <> 0 Then
            If quadrant = 1 Then
                xeins = 1
                yeins = 1
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 2 Then
                xeins = 1
                yeins = 4
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 3 Then
                xeins = 1
                yeins = 7
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 4 Then
                xeins = 4
                yeins = 1
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 5 Then
                xeins = 4
                yeins = 4
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 6 Then
                xeins = 4
                yeins = 7
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 7 Then
                xeins = 7
                yeins = 1
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 8 Then
                xeins = 7
                yeins = 4
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
            If quadrant = 9 Then
                xeins = 7
                yeins = 7
                xdrei = xeins + 2
                ydrei = yeins + 2
            End If
        End If
    End Sub
    Public Sub getbox(ByVal u As Integer, ByVal v As Integer) 'Bestimme Eckpunkte der Box anhand Koordinaten
        If u AndAlso v <> 0 Then
            'Welche row
            If u < 4 Then '1 2 3
                xeins = 1
                xdrei = xeins + 2
            End If
            If u < 7 And u > 3 Then ' 4 5 6
                xeins = 4
                xdrei = xeins + 2
            End If
            If u > 6 Then '7 8 9
                xeins = 7
                xdrei = xeins + 2
            End If
            'Welche column
            If v < 4 Then '1 2 3
                yeins = 1
                ydrei = yeins + 2
            End If
            If v < 7 And v > 3 Then ' 4 5 6
                yeins = 4
                ydrei = yeins + 2
            End If
            If v > 6 Then '7 8 9
                yeins = 7
                ydrei = yeins + 2
            End If
            'die Eckpunkte des Kästchens sind nun bestimmt, x-Werte=row, y-Werte=column


            If xeins = 1 Then
                If yeins = 1 Then
                    quadrant = 1
                End If
            End If
            If xeins = 4 Then
                If yeins = 1 Then
                    quadrant = 4
                End If
            End If
            If xeins = 7 Then
                If yeins = 1 Then
                    quadrant = 7
                End If
            End If
            If xeins = 1 Then
                If yeins = 4 Then
                    quadrant = 2
                End If
            End If
            If xeins = 4 Then
                If yeins = 4 Then
                    quadrant = 5
                End If
            End If
            If xeins = 7 Then
                If yeins = 4 Then
                    quadrant = 8
                End If
            End If
            If xeins = 1 Then
                If yeins = 7 Then
                    quadrant = 3
                End If
            End If
            If xeins = 4 Then
                If yeins = 7 Then
                    quadrant = 6
                End If
            End If
            If xeins = 7 Then
                If yeins = 7 Then
                    quadrant = 9
                End If
            End If

        End If
    End Sub

    Public Function AllFilled() As Boolean
        Dim count As Integer = 0
        For Me.u = 1 To 9
            For Me.v = 1 To 9
                If Me.GetControlByName("txt" + CStr(Me.u) + CStr(Me.v)).Text <> "" Then
                    count += 1
                End If
            Next
        Next

        If count = 81 Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Sub fill(ByVal u As Integer, ByVal v As Integer, ByVal z As Integer)
        Me.GetControlByName("txt" + CStr(u) + CStr(v)).Text = z.ToString
    End Sub

    Public Function CheckQuadrantFilled(quadrant) As Boolean ' hierkönnte man noch etwas mit 2 felndern anfangen
        zähler = 0
        GetFieldByQuadrant(quadrant)
        For x = xeins To xdrei
            For y = yeins To ydrei
                If Me.GetControlByName("txt" + CStr(x) + CStr(y)).Text = "" Then
                    zähler += 1
                    Return False
                End If
            Next
        Next
        If zähler = 1 Then
            Nur1Leer = True
        Else
            Nur1Leer = False
        End If
        Return True
    End Function


    Private Sub Button2_Click(sender As System.Object, e As System.EventArgs) Handles Button2.Click
        'Timer1.Enabled = True



        Do While AllFilled() = False
            For Me.u = 1 To 9
                For Me.v = 1 To 9
                    Randomize()
                    z = CInt(Int((9 * Rnd()) + 1))
                    If checkrow(z) AndAlso checkcolum(z) AndAlso checkbox(z) = True Then
                        fill(u, v, z)
                    End If

                    If Me.GetControlByName("txt" + CStr(Me.u) + CStr(Me.v)).Text = "" Then
                        getbox(u, v)
                        CheckQuadrantFilled(quadrant)
                        Application.DoEvents()
                        If Nur1Leer = True Then

                            Me.GetControlByName("txt" + CStr(Me.u) + CStr(Me.v)).Text = z

                            For j = 1 To 9
                                If j = u Then
                                    Continue For
                                End If
                                If Me.GetControlByName("txt" + CStr(u) + CStr(j)).Text = z.ToString Then
                                    Me.GetControlByName("txt" + CStr(u) + CStr(j)).Text = ""
                                End If
                            Next

                            For j = 1 To 9
                                If j = v Then
                                    Continue For
                                End If
                                If Me.GetControlByName("txt" + CStr(j) + CStr(v)).Text = z.ToString Then
                                    Me.GetControlByName("txt" + CStr(j) + CStr(v)).Text = ""
                                End If
                            Next
                        End If
                    End If
                Next
            Next
            UpdateArray()
            SummeVonZ()
            OnePossibility()

            If AllFilled() = True Then
                Exit Sub
            Else
                Dim a As Integer
                Dim UnVstQuadrant(8) As Boolean
                For j = 1 To 9
                    If CheckQuadrantFilled(j) = True Then
                        a += 1
                    Else
                        UnVstQuadrant(j - 1) = True
                    End If
                Next
                If a <= 2 Then
                    btnclear.PerformClick()
                Else
                    For o As Integer = 0 To 8
                        If UnVstQuadrant(o) = True Then
                            DelteQuadrant(o)
                        End If
                    Next
                End If

            End If
            UpdateArray()
            SummeVonZ()
            OnePossibility()
        Loop
        Application.DoEvents()
        UpdateArray()
        SummeVonZ()
        OnePossibility()

        'For Me.u = 1 To 9
        '    For Me.v = 1 To 9
        '        If Me.GetControlByName("txt" + CStr(Me.u) + CStr(Me.v)).Text = "" Then
        '            getbox(u,v)
        '            MsgBox(quadrant)
        '        End If
        '    Next
        'Next
    End Sub

    
    Private Function IsPossible(ByVal i As Integer) As Boolean
        UpdateArray()
        GetFieldByQuadrant(i)
        Dim count As Integer = 0
        For contingency As Integer = 0 To 8
            For x = xeins To xdrei
                For y = yeins To ydrei
                    If var(x - 1, y - 1, contingency) = 1 Then
                        count += 1
                    End If
                Next
            Next
            If count = 0 Then
                Return False
            End If
        Next
        Return True
    End Function



    Private Sub DelteQuadrant(ByVal quadrant As Integer)
        GetFieldByQuadrant(quadrant)
        For x = xeins To xdrei
            For y = yeins To ydrei
                Me.GetControlByName("txt" + CStr(x) + CStr(y)).Text = ""
            Next
        Next
    End Sub


  

    Public Sub UpdateArray()
        ListBox1.Items.Clear()
        unmög = 0
        mög = 0
        gesetzt = 0
        lbl8.Text = ""
        lbl0.Text = ""
        lbl1.Text = ""
        lbl10.Text = ""
        Dim w As Integer = 0
        Dim b As Integer = 0
        Dim n As Integer = 0
        Dim m As Integer = 0

        w = xeins
        b = xdrei
        n = yeins
        m = ydrei



        For Me.u = 1 To 9
            For Me.v = 1 To 9
                For Me.z = 1 To 9
                    If var2(u - 1, v - 1, z - 1) = "Geändert" Then
                        unmög += 1
                        Continue For
                    End If
                    'Weil diese Möglichkeit nicht nochmal getestet werden darf.
                    If Me.GetControlByName("txt" + CStr(Me.u) + CStr(Me.v)).Text <> "" Then
                        var(u - 1, v - 1, z - 1) = 10
                        gesetzt += 1
                    End If
                    If Me.GetControlByName("txt" + CStr(Me.u) + CStr(Me.v)).Text = "" Then
                        If checkrow(Me.z) = True AndAlso checkbox(Me.z) = True AndAlso checkcolum(Me.z) = True Then
                            var(u - 1, v - 1, z - 1) = 1
                            mög += 1
                        Else
                            var(u - 1, v - 1, z - 1) = 0
                            unmög += 1
                        End If
                    End If
                Next
            Next
        Next


        lbl0.Text = unmög.ToString
        lbl1.Text = mög.ToString
        lbl10.Text = gesetzt.ToString
        If (729 - (unmög + mög + gesetzt)) <> 0 Then
            lbl8.ForeColor = Color.Red
            lbl8.Text = ("Error!  729 - " & (unmög + mög + gesetzt) & " = " & 729 - (unmög + mög + gesetzt))
        Else
            lbl8.Text = ""
        End If
        unmög = 0
        mög = 0
        gesetzt = 0

        ' zwischenspeicher gebrauchen
        xeins = w
        xdrei = b
        yeins = n
        ydrei = m
    End Sub




    Private Sub btnUpdateArray_Click(sender As System.Object, e As System.EventArgs) Handles btnUpdateArray.Click
        UpdateArray()
    End Sub

    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click
        lblListBox.ForeColor = Color.Green
        lblListBox.Text = ""
        Dim unmög As Integer
        Dim mög As Integer
        Dim gesetzt As Integer
        ListBox1.FormatString = True
        For u As Integer = 0 To 8
            For v As Integer = 0 To 8
                For z As Integer = 0 To 8
                    Dim q As String = "  "
                    If var(u, v, z) = 10 Then q = ""
                    Dim s As String = "var" & "(" & u & "," & v & "," & z & ")" & " = " & q & var(u, v, z)

                    If u = 4 Then
                        If z = 5 Then
                            ListBox1.Items.Add(s & vbCrLf)
                            If var(u, v, z) = 1 Then
                                mög += 1
                            End If
                            If var(u, v, z) = 0 Then
                                unmög += 1
                            End If
                            If var(u, v, z) = 10 Then
                                gesetzt += 1
                            End If
                        End If
                    End If


                Next


            Next
        Next
       
        lblListBox.Text = "ListBox contains: " + " möglich: " & mög & " gesetzt: " & gesetzt & " unmöglich: " & unmög

    End Sub
    Private Sub Button6_Click(sender As System.Object, e As System.EventArgs) Handles Button6.Click

        lblListBox.Text = ""

        ListBox1.FormatString = True
        For u As Integer = 0 To 8
            For v As Integer = 0 To 8
                For z As Integer = 0 To 8
                    Dim s As String = "var2" & "(" & u & "," & v & "," & z & ")" & " = " & var2(u, v, z)

                    ListBox1.Items.Add(s & vbCrLf)


                Next


            Next
        Next

    End Sub


    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click
        UpdateArray()
        Try
            u = CInt(TextBox1.Text)
            v = CInt(TextBox2.Text)
            z = CInt(TextBox3.Text)
        Catch ex As Exception

        End Try

        MessageBox.Show("var(u - 1, v - 1, i - 1)" & "  =  " & (var(u - 1, v - 1, z - 1)))

    End Sub


    Private Sub btnnicestuff_Click(sender As System.Object, e As System.EventArgs) Handles btnnicestuff.Click
        btnclear.PerformClick()
        fill(1, 3, 1)
        fill(1, 4, 9)
        fill(1, 5, 5)
        fill(1, 6, 7)
        fill(1, 8, 6)
        fill(1, 9, 3)
        fill(2, 4, 8)
        fill(2, 4, 8)
        fill(2, 6, 6)
        fill(2, 8, 7)
        fill(3, 1, 7)
        fill(3, 2, 6)
        fill(3, 3, 9)
        fill(3, 4, 1)
        fill(3, 5, 3)
        fill(3, 7, 8)
        fill(3, 9, 5)
        fill(4, 3, 7)
        fill(4, 4, 2)
        fill(4, 5, 6)
        fill(4, 6, 1)
        fill(4, 7, 3)
        fill(4, 8, 5)
        fill(5, 1, 3)
        fill(5, 2, 1)
        fill(5, 3, 2)
        fill(5, 4, 4)
        fill(5, 5, 9)
        fill(5, 6, 5)
        fill(5, 7, 7)
        fill(5, 8, 8)
        fill(5, 9, 6)
        fill(6, 2, 5)
        fill(6, 3, 6)
        fill(6, 4, 3)
        fill(6, 5, 7)
        fill(6, 6, 8)
        fill(7, 1, 1)
        fill(7, 3, 8)
        fill(7, 4, 6)
        fill(7, 6, 9)
        fill(7, 7, 5)
        fill(7, 9, 7)
        fill(8, 2, 9)
        fill(8, 4, 7)
        fill(8, 5, 1)
        fill(8, 7, 6)
        fill(8, 9, 8)
        fill(9, 1, 6)
        fill(9, 2, 7)
        fill(9, 3, 4)
        fill(9, 4, 5)
        fill(9, 5, 8)
        fill(9, 6, 3)



    End Sub

    Private Sub Button5_Click(sender As System.Object, e As System.EventArgs) Handles Button5.Click
        Dim q As Integer = CInt(txtq.Text)
        Dim s As Integer = CInt(txtc.Text) - 1
        If GreenLight(q, s) <= 3 Then
            MessageBox.Show("true, 3 oder weniger candidates für diese zahl in dieser Box")
        Else
            MessageBox.Show("false, mehr als 3 candidates in dieser Box")
        End If
    End Sub


    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs) Handles Button7.Click
   
        CandidateLines()
    End Sub

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs) Handles Button8.Click
        CandidateLines()
    End Sub

   

    Function SudokuIsEmpty() As Boolean
        For Me.u = 1 To 9
            For Me.v = 1 To 9
                If Me.GetControlByName("txt" + CStr(Me.u) + CStr(Me.v)).Text <> "" Then
                    count += 1
                End If
            Next
        Next

        If count = 0 Then
            Return True
        Else
            Return False
        End If

    End Function
    Private Sub btnReset_Click(sender As System.Object, e As System.EventArgs) Handles btnReset.Click
        If AllFilled() = True Then
            Exit Sub
        End If

        btnclear.PerformClick()
        For Me.u = 0 To 8
            For Me.v = 0 To 8
                Dim i As Integer = Speicher(u, v)
                Dim s As String = ""
                fill(u + 1, v + 1, i + 1)
            Next
        Next
        For Each c As Control In TableLayoutPanel1.Controls
            If TypeOf c Is TextBox Then
                If c.Text = 0 Then
                    c.Text = ""
                End If
            End If
        Next
    End Sub

    Private Sub btnsavethis_Click(sender As System.Object, e As System.EventArgs) Handles btnsavethis.Click
        For Me.u = 0 To 8
            For Me.v = 0 To 8
                Speicher(u, v) = CInt(Val(Me.GetControlByName("txt" & CStr(u + 1) & CStr(v + 1)).Text)) - 1
                'MessageBox.Show("u" & "  " & u & "v" & "  " & v & "z" & "  " & CInt(Val(Me.GetControlByName("txt" & CStr(u + 1) & CStr(v + 1)).Text)))
            Next
        Next

    End Sub
    Private Sub btneasy_Click(sender As System.Object, e As System.EventArgs) Handles btneasy.Click

        btnclear.PerformClick()
        fill(1, 1, 9)
        fill(1, 3, 6)
        fill(2, 2, 5)
        fill(2, 1, 4)
        fill(3, 1, 8)
        fill(3, 2, 7)
        fill(3, 3, 2)
        fill(1, 4, 7)
        fill(1, 5, 4)
        fill(1, 6, 1)
        fill(2, 4, 9)
        fill(2, 6, 8)
        fill(3, 5, 6)
        fill(3, 6, 3)
        fill(1, 7, 8)
        fill(1, 9, 2)
        fill(2, 8, 6)
        fill(2, 9, 3)
        fill(3, 7, 9)
        fill(3, 8, 4)
        fill(4, 2, 1)
        fill(4, 3, 4)
        fill(5, 1, 2)
        fill(5, 3, 5)
        fill(6, 2, 8)
        fill(6, 3, 9)
        fill(4, 4, 2)
        fill(4, 5, 8)
        fill(5, 4, 3)
        fill(5, 6, 7)
        fill(6, 5, 1)
        fill(6, 6, 6)
        fill(4, 7, 6)
        fill(4, 9, 9)
        fill(5, 7, 4)
        fill(5, 8, 1)
        fill(6, 8, 2)
        fill(6, 9, 5)
        fill(7, 1, 1)
        fill(7, 2, 2)
        fill(8, 1, 5)
        fill(8, 2, 4)
        fill(9, 2, 9)
        fill(9, 3, 7)
        fill(7, 4, 6)
        fill(7, 5, 3)
        fill(8, 4, 1)
        fill(8, 6, 9)
        fill(9, 4, 8)
        fill(9, 6, 2)
        fill(7, 8, 9)
        fill(7, 9, 7)
        fill(8, 7, 2)
        fill(8, 9, 6)
        fill(9, 7, 1)
        fill(9, 8, 3)


    End Sub
    Private Sub btnmedium_Click(sender As System.Object, e As System.EventArgs) Handles btnmedium.Click
        btnclear.PerformClick()
        fill(1, 1, 1)
        fill(1, 2, 4)
        fill(1, 6, 3)
        fill(2, 5, 8)
        fill(2, 6, 4)
        fill(3, 4, 2)
        fill(3, 5, 7)
        fill(1, 7, 2)
        fill(2, 7, 9)
        fill(2, 8, 3)
        fill(4, 3, 9)
        fill(5, 1, 8)
        fill(6, 1, 5)
        fill(4, 3, 9)
        fill(6, 3, 2)
        fill(4, 5, 3)
        fill(4, 6, 2)
        fill(6, 4, 8)
        fill(6, 5, 4)
        fill(4, 7, 8)
        fill(6, 7, 6)
        fill(6, 8, 1)
        fill(5, 9, 3)
        fill(7, 5, 2)
        fill(7, 6, 8)
        fill(8, 2, 5)
        fill(8, 3, 8)
        fill(8, 4, 4)
        fill(8, 5, 9)
        fill(8, 8, 6)
        fill(9, 2, 9)
        fill(9, 3, 7)
        fill(9, 4, 3)
        fill(9, 7, 4)
        fill(9, 8, 8)
        fill(9, 9, 1)

    End Sub
    Private Sub btnmedium2_Click(sender As System.Object, e As System.EventArgs) Handles btnmedium2.Click
        btnclear.PerformClick()
        fill(1, 2, 1)
        fill(1, 4, 9)
        fill(1, 7, 8)
        fill(2, 6, 8)
        fill(2, 9, 4)
        fill(3, 1, 6)
        fill(3, 3, 5)
        fill(3, 7, 7)
        fill(4, 2, 9)
        fill(4, 5, 6)
        fill(4, 9, 8)
        fill(5, 4, 2)
        fill(5, 6, 7)
        fill(6, 1, 8)
        fill(6, 5, 3)
        fill(6, 8, 6)
        fill(7, 3, 2)
        fill(7, 7, 5)
        fill(7, 9, 3)
        fill(8, 1, 1)
        fill(8, 4, 4)
        fill(9, 3, 6)
        fill(9, 6, 2)
        fill(9, 8, 1)

    End Sub
    Private Sub btnhard2_Click(sender As System.Object, e As System.EventArgs) Handles btnhard2.Click
        btnclear.PerformClick()
        fill(1, 1, 7)
        fill(1, 4, 4)
        fill(1, 6, 1)
        fill(1, 7, 2)
        fill(2, 1, 1)
        fill(2, 8, 8)
        fill(3, 2, 9)
        fill(3, 3, 5)
        fill(3, 5, 7)
        fill(3, 6, 3)
        fill(4, 2, 5)
        fill(4, 5, 9)
        fill(4, 6, 8)
        fill(4, 9, 2)
        fill(5, 1, 8)
        fill(5, 9, 5)
        fill(6, 6, 4)
        fill(6, 7, 7)
        fill(6, 8, 3)
        fill(7, 1, 6)
        fill(7, 2, 4)
        fill(7, 3, 7)
        fill(7, 5, 8)
        fill(8, 4, 6)
        fill(8, 5, 2)
        fill(8, 6, 5)
        fill(9, 5, 4)
        fill(9, 7, 1)
    End Sub
    Private Sub Button9_Click(sender As System.Object, e As System.EventArgs) Handles Button9.Click
        btnclear.PerformClick()
        fill(1, 1, 5)
        fill(1, 3, 6)
        fill(1, 8, 7)
        fill(2, 2, 4)
        fill(2, 6, 8)
        fill(2, 7, 5)
        fill(3, 1, 3)
        fill(3, 4, 6)
        fill(3, 8, 9)
        fill(4, 1, 8)
        fill(4, 4, 5)
        fill(4, 6, 9)
        fill(5, 4, 1)
        fill(5, 5, 6)
        fill(5, 6, 7)
        fill(6, 4, 4)
        fill(6, 6, 3)
        fill(6, 9, 9)
        fill(7, 2, 5)
        fill(7, 6, 1)
        fill(7, 9, 8)
        fill(8, 3, 8)
        fill(8, 4, 9)
        fill(8, 8, 3)
        fill(9, 9, 7)
        fill(9, 2, 1)
        fill(9, 7, 4)
        fill(9, 9, 6)
        fill(1, 9, 2)
        fill(9, 1, 7)
    End Sub


    'Sudokus von http://www.raetseldino.de/sudoku-einfach.html und http://www.raetseldino.de/sudoku-vorlage-schwer.html
End Class

