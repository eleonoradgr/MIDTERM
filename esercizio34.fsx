#load "mylwc.fsx"

open System.Windows.Forms
open System.Drawing
open Mylwc

let f = new Form(Text="MidTerm", TopMost=true)
f.Show()

type Letter () as this =
    let mutable location = PointF()
    let mutable size= SizeF(26.f,32.f)

    let mutable char = string()
    let mutable font = Font("Arial", 18.f)
    let mutable color = Color.Black
    let mutable mat = new W2V()
    let mutable selected = true

    member this.Char
        with get() = char
        and set(v) = char<-v
    member this.Location
        with get() = location
        and set(v) = location <- v;
    member this.Size
        with get() = size
        and set(v) = size <- v;
    member this.Font
        with get() = font
        and set(v) = font <- v;
    member this.Color
        with get() = color
        and set(v) = color <- v;
    member this.Mat
        with get() = mat
        and set(v) = mat <- v;
    member this.Selected
        with get() = selected
        and set(v) = selected <- v;
    
    member this.Paint(g:Graphics) =
        let ctx= g.Save()
        let s = 
            if (not (this.Char.Equals "" )) then
                g.MeasureString(this.Char, this.Font)
            else this.Size
        g.Transform <- this.Mat.W2V
        if(this.Selected) then
            g.DrawRectangle(Pens.Black, new Rectangle(PointFtoPoint(this.Location),SizeFtoSize(s)))
        g.DrawString(this.Char, this.Font, new SolidBrush(this.Color), this.Location)
        g.Restore(ctx)

type Editor() as this=
    inherit LWContainer()

    let buttons=[|
        new Rectbutton(Text="Add", Location=PointF(16.f, 0.f));
        new Rectbutton(Text="Delete", Location= PointF(16.f, 64.f ));
        new Rectbutton(Text="Select all", Location=PointF(16.f, 128.f));
    |]

    let colorButtons= [|
        new CirButtons(Text="B", Color= Black,Location=PointF(32.f,194.f))
        new CirButtons(Text="B", Location=PointF(32.f,226.f))
        new CirButtons(Text="R", Location=PointF(32.f,258.f))
    |]


    let mutable newl = Letter()
    let mutable letters = ResizeArray<Letter>();
    let mutable startDrawing = PointF(112.f, 32.f)
    let mutable havetodraw=false

    let mutable lselected= null
    let mutable lTimer= new Timer(Interval=450)
    let mutable line=true; 

    do this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
    do 
        this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
        buttons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        colorButtons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        lTimer.Tick.Add( fun _ ->
            if line then
                line<-false
            else line<-true
            this.Invalidate()
        )
        buttons.[0].Click.Add(fun _ ->
            havetodraw<-true
            newl<- new Letter(Location= startDrawing)
            lTimer.Start()
            this.Invalidate()  
        )
        buttons.[1].Click.Add( fun _ ->
            for i in [letters.Count-1..0] do
                if letters.[i].Selected then
                   letters.RemoveAt(i)
            this.Invalidate()
        )
        buttons.[2].Click.Add( fun _ ->
            for l in letters do 
                l.Selected<-true
            this.Invalidate()
        )
    
    override this.OnKeyDown e =
        if (havetodraw && e.KeyValue>59 && e.KeyValue<91) then
            havetodraw<-false

            letters.Add(new Letter(Char=e.KeyCode.ToString(),Location=startDrawing))

    override this.OnPaint(e) =
    let g= e.Graphics
    if (havetodraw) then
        newl.Paint(g)
        if line then 
            let s = new PointF(startDrawing.X+ 2.f, startDrawing.Y+2.f)
            let e= new PointF(s.X, s.Y+28.f)
            g.DrawLine(Pens.Black, s, e)
    for l in letters do
        l.Paint(g)
    base.OnPaint(e)




let p = new Editor(Dock=DockStyle.Fill)
f.Controls.Add(p)
p.Focus()






