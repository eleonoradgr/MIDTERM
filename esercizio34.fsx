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
    let mutable font = new Font("Arial", 18.f)
    let mutable color = Color.Black
    let mutable uppercase= true
    let mutable mat =  W2V()
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
    
    member this.Center =
        PointF(this.Location.X+(this.Size.Width/2.f),this.Location.Y+(this.Size.Height/2.f )) 

    member this.TransformP (m:Drawing2D.Matrix) (p:Point) =
        let a = [| PointF(single p.X, single p.Y) |]
        m.TransformPoints(a)
        a.[0]
    member this.IncreaseSize =
        let h=font.GetHeight()
        let nf= new Font(this.Font.FontFamily, (this.Font.Size + 1.f))
        let nh= nf.GetHeight()
        this.Location<-PointF(this.Location.X - ((nh-h)/2.f),this.Location.Y - ((nh-h)/2.f)) 
        this.Font<-nf
    
    member this.DecreaseSize =
        let h=font.GetHeight()
        let mutable sz= this.Font.Size - 1.f
        if(sz <= 0.f) then
            sz<- 1.f
        let nf= new Font(this.Font.FontFamily, sz)
        let nh= nf.GetHeight()
        this.Location<-PointF(this.Location.X + ((h-nh)/2.f),this.Location.Y + ((h-nh)/2.f)) 
        this.Font<-nf

    member this.ChangeCase(s:string) =
        let mutable b = this.Char.ToCharArray(0,1)
        let c =int( b.[0])
        if(  c >= 65 && c <=90 && (s.Equals "lower")) then
            this.Char<- this.Char.ToLower()
        else 
            if ( c>= 97 && c<=122 && (s.Equals "UPPER")) then
                this.Char<- this.Char.ToUpper()


    member this.HitTest(p:Point) =
        let loc = this.TransformP this.Mat.V2W p
        Rectangle(PointFtoPoint(this.Location), SizeFtoSize(this.Size)).Contains(Point(int(loc.X),int(loc.Y)))
        

    
    member this.Paint(g:Graphics) =
        let ctx= g.Save()
        let aux= g.Transform.Clone()
        aux.Multiply(this.Mat.W2V)
        g.Transform <- aux
        let s = 
            if (not (this.Char.Equals "" )) then
                g.MeasureString(this.Char, this.Font)
            else this.Size
        this.Size<-s 
        if(this.Selected) then
            g.DrawRectangle(Pens.Black, Rectangle(PointFtoPoint(this.Location),SizeFtoSize(s)))
        g.DrawString(this.Char, this.Font, new SolidBrush(this.Color), this.Location)
        g.Restore(ctx)

type Editor() as this=
    inherit LWContainer()

    let buttons=[|
        Rectbutton(Text="Set Start Point", Location=PointF(8.f, 0.f), Size= SizeF(80.f,32.f) )
        Rectbutton(Text="Add", Location=PointF(16.f, 40.f));
        Rectbutton(Text="Delete", Location= PointF(16.f, 80.f ));
        Rectbutton(Text="Select all", Location=PointF(16.f, 120.f));
    |]
    let buttonUL= Rectbutton(Text="UPPER",Location=PointF(16.f,160.f))
    let fontButtons=[|
        Rectbutton(Text="Monospace",Location=PointF(16.f, 200.f),Font= new Font(FontFamily.GenericMonospace, 20.f), Selected=true);
        Rectbutton(Text="SansSerif", Location=PointF(16.f, 240.f), Font= new Font(FontFamily.GenericSansSerif,20.f));
        Rectbutton(Text="Serif", Location=PointF(16.f, 280.f), Font= new Font(FontFamily.GenericSerif,20.f));
    |]

    let colorButtons= [|
        CirButtons(Text="K", Color= Color.Black,Location=PointF(32.f,320.f), Selected=true);
        CirButtons(Text="B", Color=Color.Blue, Location=PointF(32.f,360.f));
        CirButtons(Text="R", Color=Color.Red,Location=PointF(32.f,400.f));
    |]

    let moveButtons = [| 
        CirButtons(Text="U",Color= Color.Gray, Location=PointF(32.f, 440.f));
        CirButtons(Text="R",Color= Color.Gray,Location=PointF(64.f, 472.f));
        CirButtons(Text="L",Color= Color.Gray,Location=PointF(0.f, 472.f));
        CirButtons(Text="D",Color= Color.Gray,Location=PointF(32.f, 504.f));
        CirButtons(Text="<-",Color= Color.Gray,Location=PointF(0.f, 434.f));
        CirButtons(Text="->",Color= Color.Gray,Location=PointF(64.f, 434.f));
        CirButtons(Text="+",Color= Color.Gray,Location=PointF(0.f, 510.f));
        CirButtons(Text="-",Color= Color.Gray,Location=PointF(64.f, 510.f));
    |]

    let animationButton = Rectbutton(Text="Start Animation", Location=PointF(8.f, 560.f), Size= SizeF(80.f,32.f) )
    let mutable newl = Letter() //per inserimento nuova lettera
    let mutable letters = ResizeArray<Letter>();//insieme di tutte le lettere presenti
    let mutable startPoint = PointF(112.f, 32.f) // punto dal quale viene considerato l'inserimento di letere successive
    let mutable startDrawing = PointF(112.f, 32.f) //punto di inserimento nuova lettera
    let mutable havetodraw=false //true se si sta scrivendo una nuova lettera, false altrimenti

    let mutable lselected= -1 //li se non è selezionata nessuna lettera, indice in letters altrimenti
    
    let mutable line=true;
    let mutable lTimer= new Timer(Interval=450) //timer per tick scrittura lettera ed help message
    let mutable aus= 4
    let mutable helpTimer= new Timer(Interval=1000) //timer per messaggio di aiuto

    let mutable tick = 0
    let mutable aus2 = 1.f
    let mutable animationTimer= new Timer(Interval= 100) // Timer per l'animazione

    let mutable drag=false
    let mutable offset= PointF(0.f, 0.f)
    
    let mutable scrollDir=""
    let moving s =
        match s with
        |"U"->
            if lselected >= 0 then
                letters.[lselected].Mat.Translate(0.f,-10.f)
            else 
                if lselected = -2 then
                    for l in letters do
                        l.Mat.Translate(0.f,-10.f)
                else this.Transform.Translate(0.f,-10.f)
            this.Invalidate()
        |"R"->
            if lselected >= 0 then
                letters.[lselected].Mat.Translate(10.f,0.f)
            else 
                if lselected = -2 then
                    for l in letters do
                        l.Mat.Translate(10.f,0.f)
                else this.Transform.Translate(10.f,0.f)
            this.Invalidate()
        |"L"->
            if lselected >= 0 then
                letters.[lselected].Mat.Translate(-10.f,0.f)
            else 
                if lselected = -2 then
                    for l in letters do
                        l.Mat.Translate(-10.f,0.f)
                else this.Transform.Translate(-10.f,0.f)
            this.Invalidate()
        |"D"->
            if lselected >= 0 then
                letters.[lselected].Mat.Translate(0.f,10.f)
            else
                if lselected = -2 then
                    for l in letters do
                        l.Mat.Translate(0.f,10.f)
                else this.Transform.Translate(0.f,10.f)
            this.Invalidate()
        |"<-"->
            if lselected >= 0 then
                let p= letters.[lselected].Center
                letters.[lselected].Mat.RotateAtCenter(-10.f, p)
            else 
                if lselected = -2 then
                    for l in letters do
                        let p= l.Center
                        l.Mat.RotateAtCenter(-10.f, p) 
                else //this.Transform.RotateAtCenter(-10.f,Point2PointF( Point(this.Size.Width/2, this.Size.Height/2)))
                    let p= TransformPoint this.Transform.V2W (Point2PointF( Point(this.Size.Width/2, this.Size.Height/2))) 
                    this.Transform.RotateAtCenter(-10.f,p)
            this.Invalidate()
        |"->"->
            if lselected >= 0 then
                let p= letters.[lselected].Center
                letters.[lselected].Mat.RotateAtCenter(10.f, p)
            else
                if lselected = -2 then
                    for l in letters do 
                        let p= l.Center
                        l.Mat.RotateAtCenter(10.f, p)
                else
                    let p= TransformPoint this.Transform.V2W (Point2PointF( Point(this.Size.Width/2, this.Size.Height/2))) 
                    this.Transform.RotateAtCenter(10.f,p)
            this.Invalidate()
        |"+"->
            if lselected >= 0 then
                letters.[lselected].IncreaseSize
            else //this.Transform.Scale(1.1f,1.1f)
                let p= TransformPoint this.Transform.V2W (Point2PointF( Point(this.Size.Width/2, this.Size.Height/2))) 
                this.Transform.ScaleAtCenter(1.1f,1.1f,p)
            this.Invalidate()
        |"-"->
            if lselected >= 0 then
                letters.[lselected].DecreaseSize
            else //this.Transform.Scale(1.f/1.1f,1.f/1.1f)
                let p= TransformPoint this.Transform.V2W (Point2PointF( Point(this.Size.Width/2, this.Size.Height/2))) 
                this.Transform.ScaleAtCenter(1.f/1.1f,1.f/1.1f,p)
            this.Invalidate()
        |_->()



    do this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
    do 
        this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
        buttons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        buttonUL.Parent<-this; this.LWControls.Add(buttonUL)
        fontButtons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        colorButtons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        moveButtons |> Seq.iter (fun b -> b.Parent <- this; this.LWControls.Add(b))
        animationButton.Parent<-this; this.LWControls.Add(animationButton)
        lTimer.Tick.Add( fun _ ->
            if line then
                line<-false
            else line<-true
            this.Invalidate()
        )
        helpTimer.Tick.Add(fun _ ->
            if(aus = 0) then
                aus<- 4
                helpTimer.Stop()
            else aus<- aus - 1
            this.Invalidate()
        )
        animationTimer.Tick.Add(fun _->
            //if(tick < 3) then
                //for l in letters do
                    //l.Mat.RotateAtCenter(30.f, l.Center)
                    //l.Mat.Translate(-20.f, -10.f)
            if( tick>=(3*int(aus2)) && tick<(12*int(aus2))) then
                for l in letters do
                    l.Mat.RotateAtCenter(-10.f/aus2, l.Center)
                    l.Mat.Translate(10.f*aus2, 0.f)
            if( tick>=(12*int(aus2)) && tick<(21*int(aus2))) then
                for l in letters do
                    l.Mat.RotateAtCenter(-10.f/aus2, l.Center)
                    l.Mat.Translate(-10.f*aus2, 0.f)
            if (tick>=(21*int(aus2)) && tick <(30*int(aus2))) then
                for l in letters do
                    l.Mat.RotateAtCenter(-10.f/aus2, l.Center)
                    l.Mat.Translate(10.f*aus2, 0.f)
            if (tick>=(30*int(aus2)) && tick <(39*int(aus2))) then
                for l in letters do
                    l.Mat.RotateAtCenter(-10.f/aus2, l.Center)
                    l.Mat.Translate(-10.f*aus2, 0.f)
            let mutable l= (39*int(aus2))
            //if(tick >= l && tick < l+3) then
                //for l in letters do
                    //l.Mat.RotateAtCenter(-30.f, l.Center)
                    //l.Mat.Translate(20.f*aus2, 10.f)
            if(tick>= l+3 && tick< (l+3+54)) then
                for l in letters do
                    l.Mat.RotateAtCenter(-20.f, l.Center)
                animationTimer.Interval<-animationTimer.Interval*(2/3)+1
            tick <- tick + 1
            l<- (l+3+54)
            if(tick >= l && aus2 = 1.f) then
                tick<- 0 
                aus2<- 2.f
                animationTimer.Interval<- 80
            else if(tick >=l && aus2 = 2.f) then
                    tick<-0
                    aus2<-1.f
                    animationTimer.Interval<-100
            this.Invalidate()
        )
        buttons.[0].Click.Add(fun _ ->
            helpTimer.Start()
            this.Invalidate()
        )
        buttons.[1].Click.Add(fun _ ->
            for l in letters do
                l.Selected<-false
            lselected<- -1
            havetodraw<-true
            newl<- Letter(Location= startDrawing)
            lTimer.Start()
            this.Invalidate()  
        )
        buttons.[2].Click.Add( fun _ ->
            
            if (lselected >= 0) then
                startDrawing<-letters.[lselected].Location
                letters.RemoveAt(lselected)
            else if (lselected = -2) then
                    letters.Clear()
                    startDrawing<-startPoint
                 else  havetodraw<-false   
            lselected<- -1
            this.Invalidate()
        )
        buttons.[3].Click.Add( fun _ ->
            for l in letters do 
                l.Selected<-true
            lselected<- -2
            havetodraw<-false
            this.Invalidate()
        )
        buttonUL.Click.Add( fun _ ->
            if (buttonUL.Text.Equals "lower") then
                buttonUL.Text <- "UPPER"
            else buttonUL.Text<- "lower"
            if lselected >=0 then
                letters.[lselected].ChangeCase( buttonUL.Text )
            else 
                if lselected = -2 then
                    for l in letters do
                        l.ChangeCase(buttonUL.Text)
        ) 
        fontButtons.[0].Click.Add( fun _ ->
            for f in fontButtons do
                f.Selected<-false
            fontButtons.[0].Selected<-true
            if(lselected >= 0) then
                letters.[lselected].Font <- new Font( fontButtons.[0].Font.FontFamily, letters.[lselected].Font.Size)
            else
                if lselected = -2 then
                    for l in letters do
                        l.Font <- new Font( fontButtons.[0].Font.FontFamily, l.Font.Size)
            this.Invalidate()
            
        )
        fontButtons.[1].Click.Add( fun _ ->
            for f in fontButtons do
                f.Selected<-false
            fontButtons.[1].Selected<-true
            if(lselected >= 0) then
                letters.[lselected].Font <- new Font( fontButtons.[1].Font.FontFamily, letters.[lselected].Font.Size)
            else
                if lselected = -2 then
                    for l in letters do
                        l.Font <- new Font( fontButtons.[1].Font.FontFamily, l.Font.Size)
            this.Invalidate()
        )
        fontButtons.[2].Click.Add( fun _ ->
            for f in fontButtons do
                f.Selected<-false
            fontButtons.[2].Selected<-true
            if(lselected >= 0) then
                letters.[lselected].Font <- new Font( fontButtons.[2].Font.FontFamily, letters.[lselected].Font.Size)
            else
                if lselected = -2 then
                    for l in letters do
                        l.Font <- new Font( fontButtons.[2].Font.FontFamily, l.Font.Size)
            this.Invalidate()
        )
        colorButtons.[0].Click.Add( fun _ ->
            for c in colorButtons do
                c.Selected<-false
            colorButtons.[0].Selected<-true
            if(lselected >= 0) then
                letters.[lselected].Color <- colorButtons.[0].Color
            else
                if lselected = -2 then
                    for l in letters do
                        l.Color <- colorButtons.[0].Color
            this.Invalidate()
        )
        colorButtons.[1].Click.Add( fun _ ->
            for c in colorButtons do
                c.Selected<-false
            colorButtons.[1].Selected<-true
            if(lselected >= 0) then
                letters.[lselected].Color <- colorButtons.[1].Color
            else
                if lselected = -2 then
                    for l in letters do
                        l.Color <- colorButtons.[1].Color
            this.Invalidate()
        )
        colorButtons.[2].Click.Add( fun _ ->
            for c in colorButtons do
                c.Selected<-false
            colorButtons.[2].Selected<-true
            if(lselected >= 0) then
                letters.[lselected].Color <- colorButtons.[2].Color
            else
                if lselected = -2 then
                    for l in letters do
                        l.Color <- colorButtons.[2].Color
            this.Invalidate()
        )
        moveButtons.[0].Click.Add( fun _ ->
            scrollDir<- "U"
            moving scrollDir
        )
        moveButtons.[1].Click.Add( fun _ ->
            scrollDir<- "R"
            moving scrollDir
        )
        moveButtons.[2].Click.Add( fun _ ->
            scrollDir<- "L"
            moving scrollDir
        )
        moveButtons.[3].Click.Add( fun _ ->
            scrollDir<- "D"
            moving scrollDir
        )
        moveButtons.[4].Click.Add( fun _ ->
            scrollDir<- "<-"
            moving scrollDir
        )
        moveButtons.[5].Click.Add( fun _ ->
            scrollDir<- "->"
            moving scrollDir
        )
        moveButtons.[6].Click.Add( fun _ ->
            scrollDir<- "+"
            moving scrollDir
        )
        moveButtons.[7].Click.Add( fun _ ->
            scrollDir<- "-"
            moving scrollDir
        )
        
        animationButton.Click.Add (fun _ ->
            if (animationButton.Text = "Start Animation") then
                animationButton.Text<- "Stop Animation"
                tick<- 0
                aus2<-1.f
                animationTimer.Start()
            else 
                animationButton.Text <-"Start Animation"
                animationTimer.Stop()
                tick<-0
                aus2<-1.f  
        )
    member this.GetSelectedFont =
        let index= fontButtons |> Seq.tryFindIndex(fun f -> f.Selected)
        match index with
            |Some idx ->
                fontButtons.[idx].Font
            |_ -> new Font(FontFamily.GenericMonospace, 12.f)
    member this.GetSelectedColor =
        let index= colorButtons |> Seq.tryFindIndex(fun c -> c.Selected)
        match index with
            |Some idx ->
                colorButtons.[idx].Color
            |_ -> Color.Black
    member this.ChangeFont(f:Font) =
        for b in fontButtons do
            if b.Font.Equals f then 
                b.Selected<- true
            else b.Selected<-false
    member this.ChangeColor(c:Color) =
        for b in colorButtons do
            if b.Color.Equals c then
                b.Selected<-true     
            else b.Selected<-false
    member this.ChangeCase (b) =
        if (b>= 97 && b<=122) then
            buttonUL.Text<-"lower"
        else buttonUL.Text<-"UPPER"
         
    member this.InButtons(p:Point) =
        let mutable x = false
        for b in buttons do
            x<- x|| b.HitTest(Point2PointF(p))
        for b in fontButtons do
            x<- x|| b.HitTest(Point2PointF(p))
        for b in colorButtons do
            x<-x || b.HitTest(Point2PointF(p))
        for b in moveButtons do
            x<-x || b.HitTest(Point2PointF(p))
        x|| buttonUL.HitTest(Point2PointF(p))
        
    override this.OnKeyDown e =
        if (not havetodraw) then
            match e.KeyCode with
            | Keys.W ->
                scrollDir<- "U"
                moving scrollDir
            | Keys.A ->
                scrollDir<- "L"
                moving scrollDir
            | Keys.S ->
                scrollDir<- "D"
                moving scrollDir
            | Keys.D ->
                scrollDir<- "R"
                moving scrollDir
            | Keys.Q->
                scrollDir<- "<-"
                moving scrollDir
            | Keys.E ->
                scrollDir<- "->"
                moving scrollDir
            | Keys.Z ->
                scrollDir<- "+"
                moving scrollDir
            | Keys.X ->
                scrollDir<- "-"
                moving scrollDir
            |_-> ()
        if (havetodraw && (e.KeyValue>59 && e.KeyValue<91)  ) then
            havetodraw<-false
            let mutable s = e.KeyCode.ToString()
            if(buttonUL.Text.Equals "UPPER"&& e.KeyValue>59 && e.KeyValue<91) then
                s<- s.ToUpper()
            else s<-s.ToLower()
            letters.Add( Letter(Char=s,Location= startDrawing, Font= this.GetSelectedFont,Color=this.GetSelectedColor))
            lselected<-letters.Count - 1 
            startDrawing<-PointF(startDrawing.X+26.f, startDrawing.Y)
        if ( havetodraw && (e.KeyValue>47 && e.KeyValue<58)) then
            havetodraw<-false
            let mutable s = e.KeyCode.ToString()
            s<- s.Substring(1)
            letters.Add( Letter(Char=s,Location= startDrawing, Font= this.GetSelectedFont,Color=this.GetSelectedColor))
            lselected<-letters.Count - 1 
            startDrawing<-PointF(startDrawing.X+26.f, startDrawing.Y)
        if (havetodraw && e.KeyValue =32) then
            havetodraw<-false
            startDrawing<-PointF(startDrawing.X+26.f, startDrawing.Y)
            lselected<- -1
        if (havetodraw && e.KeyValue =13) then
            havetodraw<-false
            startDrawing<-PointF(startPoint.X, startDrawing.Y+32.f)
            lselected<- -1
       
        printfn "%d" e.KeyValue
        
    override this.OnMouseDown e =
        base.OnMouseDown(e)
        let l= e.Location
        let mutable l1 = TransformPoint  this.Transform.V2W  (Point2PointF(l))
        if (e.Button.Equals(MouseButtons.Left) && not (this.InButtons(l))) then
            havetodraw<-false
            let lettersrev=Seq.rev(letters)
            let nrletters=letters.Count
            let letselected = lettersrev |> Seq.tryFindIndex(fun c->
                c.HitTest( PointFtoPoint(l1) ) )
            match letselected with 
            |Some idx ->
                for c in letters do
                    c.Selected<-false
                letters.[nrletters-idx-1].Selected<-true
                lselected<-nrletters-idx-1
                this.ChangeFont(letters.[lselected].Font)
                this.ChangeColor(letters.[lselected].Color)
                let mutable by = letters.[lselected].Char.ToCharArray(0,1)
                this.ChangeCase(int(by.[0]))
                l1<- TransformPoint letters.[lselected].Mat.V2W l1
                drag<-true
                offset<- PointF(l1.X - letters.[lselected].Location.X,l1.Y - letters.[lselected].Location.Y)
                this.Invalidate()
            |_ ->
                for c in letters do
                    c.Selected<-false
                    lselected<- -1
                this.Invalidate()
        if (e.Button.Equals(MouseButtons.Right)&& not (this.InButtons(l))) then
            startDrawing<- l1
            startPoint<- l1
            for l in letters do
                l.Selected<-false
            lselected<- -1
            havetodraw<-true
            newl<- Letter(Location= startDrawing)
            lTimer.Start()
            this.Invalidate()

    override this.OnMouseMove e =
        if(drag && lselected >= 0) then
            base.OnMouseDown(e)
            let l= e.Location
            let mutable l1 = TransformPoint  this.Transform.V2W  (Point2PointF(l))
            l1<- TransformPoint letters.[lselected].Mat.V2W l1
            letters.[lselected].Location<- PointF(l1.X-offset.X, l1.Y-offset.Y)
            this.Invalidate()
        base.OnMouseMove(e)
    
    override this.OnMouseUp e =
        if (drag && lselected >= 0) then
            drag<-false
        base.OnMouseUp(e)


    override this.OnPaint(e) =
        base.OnPaint(e)
        let g= e.Graphics
        let ctx = g.Save()
        if (helpTimer.Enabled) then
            g.DrawString("Right Click every time you want select where start writing", new Font(FontFamily.GenericMonospace, 8.f),Brushes.Black, PointF(90.f, 10.f))
        g.Transform<- this.Transform.W2V
        if (havetodraw) then
            newl.Paint(g)
            if line then 
                let s = PointF(startDrawing.X+ 2.f, startDrawing.Y+2.f)
                let e= PointF(s.X, s.Y+28.f)
                g.DrawLine(Pens.Black, s, e)
        for l in letters do
            l.Paint(g)
        g.Restore(ctx)
        //base.OnPaint(e)






let p = new Editor(Dock=DockStyle.Fill)
f.Controls.Add(p)
p.Focus()


 




