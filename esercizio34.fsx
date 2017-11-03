#load "mylwc.fsx"

open System.Windows.Forms
open System.Drawing
open Mylwc

let f = new Form(Text="MidTerm", TopMost=true)
f.Show()

type Letter () as this =
    let mutable location = PointF()
    let mutable size= SizeF()

    let mutable char = char
    let mutable font = Font()
    let mutable color = Color()
    let mutable mat = new W2V()
    let mutable selected = true

    member this.Location
        with get() = location
        and set(v) = location <- v;
    member this.Size
        with get() = size
        and set(v) = size <- v;
    member this.Font
        with get() = size
        and set(v) = size <- v;
    member this.Color
        with get() = color
        and set(v) = color <- v;
    member this.Mat
        with get() = mat
        and set(v) = mat <- v;
    member this.Selected
        with get() = mat
        and set(v) = mat <- v;


type Editor() as this=
    inherit LWContainer()

    





