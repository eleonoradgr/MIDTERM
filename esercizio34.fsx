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
    let mutable uppercase= true
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
    member this.Uppercase
        with get() = uppercase
        and set(v) =uppercase<- v
    member this.Mat
        with get() = mat
        and set(v) = mat <- v;
    member this.Selected
        with get() = selected
        and set(v) = selected <- v;
    member this.transformP (m:Drawing2D.Matrix) (p:Point) =
        let a = [| PointF(single p.X, single p.Y) |]
        m.TransformPoints(a)
        a.[0]
    
    member this.HitTest(p:Point) =
        let loc = this.transformP this.Mat.V2W p
        Rectangle(PointFtoPoint(this.Location), SizeFtoSize(this.Size)).Contains(Point(int(loc.X),int(loc.Y)))
        

    
    member this.Paint(g:Graphics) =
        let ctx= g.Save()
        let s = 
            if (not (this.Char.Equals "" )) then
                g.MeasureString(this.Char, this.Font)
            else this.Size
        this.Size<-s 
        g.Transform <- this.Mat.W2V
        if(this.Selected) then
            g.DrawRectangle(Pens.Black, new Rectangle(PointFtoPoint(this.Location),SizeFtoSize(s)))
        g.DrawString(this.Char, this.Font, new SolidBrush(this.Color), this.Location)
        g.Restore(ctx)

type Editor() as this=
    inherit LWContainer()

    let buttons=[|
        new Rectbutton(Text="Add", Location=PointF(16.f, 0.f));
        new Rectbutton(Text="Delete", Location= PointF(16.f, 50.f ));
        new Rectbutton(Text="Select all", Location=PointF(16.f, 100.f));
    |]
    let buttonUL= new Rectbutton(Text="Upper",Location=PointF(16.f,150.f))
    let fontButtons=[|
        new Rectbutton(Text="Monospace",Location=PointF(16.f, 200.f),Font= Font(FontFamily.GenericMonospace, 20.f), Selected=true);
        new Rectbutton(Text="SansSerif", Location=PointF(16.f, 250.f), Font= new Font(FontFamily.GenericSansSerif,20.f));
        new Rectbutton(Text="Serif", Location=PointF(16.f, 300.f), Font= new Font(FontFamily.GenericSerif,20.f));
    |]

    let colorButtons= [|
        new CirButtons(Text="B", Color= Color.Black,Location=PointF(32.f,350.f), Selected=true);
        new CirButtons(Text="B", Color=Color.Blue, Location=PointF(32.f,400.f));
        new CirButtons(Text="R", Color=Color.Red,Location=PointF(32.f,450.f));
    |]


    let mutable newl = Letter()
    let mutable letters = ResizeArray<Letter>();
    let mutable startDrawing = PointF(112.f, 32.f)
    let mutable havetodraw=false

    let mutable lselected= -1
    let mutable lTimer= new Timer(Interval=450)
    let mutable line=true;

    

    do this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
    do 
        this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
        buttons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        buttonUL.Parent<-this; this.LWControls.Add(buttonUL)
        fontButtons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        colorButtons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        lTimer.Tick.Add( fun _ ->
            if line then
                line<-false
            else line<-true
            this.Invalidate()
        )
        buttons.[0].Click.Add(fun _ ->
            for l in letters do
                l.Selected<-false
            havetodraw<-true
            newl<- new Letter(Location= startDrawing)
            lTimer.Start()
            this.Invalidate()  
        )
        buttons.[1].Click.Add( fun _ ->
            for i in [letters.Count-1..0] do
                if letters.[i].Selected then
                   startDrawing<-letters.[i].Location
                   letters.RemoveAt(i)
                if letters.Count.Equals 0 then
                   startDrawing<-PointF(112.f, 32.f) 
            lselected<- -1
            this.Invalidate()
        )
        buttons.[2].Click.Add( fun _ ->
            for l in letters do 
                l.Selected<-true
            lselected<- -1
            this.Invalidate()
        )
        buttonUL.Click.Add( fun _ ->
            buttonUL.Text<-"lower"
        ) 
        fontButtons.[0].Click.Add( fun _ ->
            for f in fontButtons do
                f.Selected<-false
            fontButtons.[0].Selected<-true
            this.Invalidate()
        )
        fontButtons.[1].Click.Add( fun _ ->
            for f in fontButtons do
                f.Selected<-false
            fontButtons.[1].Selected<-true
            this.Invalidate()
        )
        fontButtons.[2].Click.Add( fun _ ->
            for f in fontButtons do
                f.Selected<-false
            fontButtons.[2].Selected<-true
            this.Invalidate()
        )
        colorButtons.[0].Click.Add( fun _ ->
            for c in colorButtons do
                c.Selected<-false
            colorButtons.[0].Selected<-true
            this.Invalidate()
        )
        colorButtons.[1].Click.Add( fun _ ->
            for c in colorButtons do
                c.Selected<-false
            colorButtons.[1].Selected<-true
            this.Invalidate()
        )
        colorButtons.[2].Click.Add( fun _ ->
            for c in colorButtons do
                c.Selected<-false
            colorButtons.[2].Selected<-true
            this.Invalidate()
        )
        
    member this.getSelectedFont =
        let index= fontButtons |> Seq.tryFindIndex(fun f -> f.Selected)
        match index with
            |Some idx ->
                fontButtons.[idx].Font
            |_ -> Font(FontFamily.GenericMonospace, 12.f)
    member this.getSelectedColor =
        let index= colorButtons |> Seq.tryFindIndex(fun c -> c.Selected)
        match index with
            |Some idx ->
                colorButtons.[idx].Color
            |_ -> Color.Black
    member this.ChangeFont(f:Font) =
        for b in fontButtons do
            match b.Font. with
            |f ->
                b.Selected<-true
            | _ ->
                b.Selected<-false
    member this.ChangeColor(c:Color) =
        for b in colorButtons do
            match b.Color with
            |c ->
                b.Selected<-true
            | _ ->
                b.Selected<-false
            
    member this.InButtons(p:Point) =
        let mutable x = false
        for b in buttons do
            x<- x|| b.HitTest(Point2PointF(p))
        for b in fontButtons do
            x<- x|| b.HitTest(Point2PointF(p))
        for b in colorButtons do
            x<-x || b.HitTest(Point2PointF(p))
        x|| buttonUL.HitTest(Point2PointF(p))
        
    override this.OnKeyDown e =
        if (havetodraw && e.KeyValue>59 && e.KeyValue<91) then
            havetodraw<-false
            letters.Add(new Letter(Char=(e.KeyCode.ToString()),Location=startDrawing, Font= this.getSelectedFont,Color=this.getSelectedColor))
            lselected<-letters.Count - 1
            startDrawing<-PointF(startDrawing.X+26.f, startDrawing.Y)
        if (havetodraw && e.KeyValue =32) then
            havetodraw<-false
            startDrawing<-PointF(startDrawing.X+26.f, startDrawing.Y)
            lselected<- -1
        if (havetodraw && e.KeyValue =32) then
            havetodraw<-false
            startDrawing<-PointF(26.f, startDrawing.Y+32.f)
            lselected<- -1
        printfn "%d" e.KeyValue
        
    override this.OnMouseDown e =
        base.OnMouseDown(e)
        let l= e.Location

        if (e.Button.Equals(MouseButtons.Left) && not (this.InButtons(l))) then
            havetodraw<-false
            let lettersrev=Seq.rev(letters)
            let nrletters=letters.Count
            let letselected = lettersrev |> Seq.tryFindIndex(fun c->
                c.HitTest(l))
            match letselected with 
            |Some idx ->
                for c in letters do
                    c.Selected<-false
                letters.[nrletters-idx-1].Selected<-true
                lselected<-nrletters-idx-1
                this.ChangeFont(letters.[lselected].Font)
                this.ChangeColor(letters.[lselected].Color)
                this.Invalidate()
            |_ ->
                for c in letters do
                    c.Selected<-false
                this.Invalidate()

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






