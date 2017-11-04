open System.Windows.Forms
open System.Drawing

type W2V() =
  let w2v = new Drawing2D.Matrix()
  let v2w = new Drawing2D.Matrix()

  member this.Translate(tx, ty) =
    w2v.Translate(tx, ty)
    v2w.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.Rotate(a) =
    w2v.Rotate(a)
    v2w.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.Scale(sx, sy) =
    w2v.Scale(sx, sy)
    v2w.Scale(1.f/sx, 1.f/sy, Drawing2D.MatrixOrder.Append)
  
  member this.W2V with get() = w2v
  member this.V2W with get() = v2w

let Point2PointF (p:Point) =
    PointF( float32(p.X), float32(p.Y))
let PointFtoPoint (p:PointF) =
    Point(int(p.X), int(p.Y))

let Size2SizeF (s:Size) =
    SizeF( float32(s.Width), float32(s.Height))
let SizeFtoSize (s:SizeF) =
    Size(int(s.Width), int(s.Height))
let Rect2RectF (r:Rectangle) =
  RectangleF(single r.X, single r.Y, single r.Width, single r.Height)

let RectF2Rect (r:RectangleF) =
  Rectangle(int r.X, int r.Y, int r.Width, int r.Height)

type CoordinateType = View | World

type LWControl() =
  let mutable coordinates = View

  let mutable location = PointF()
  let mutable size = SizeF()

  let mutable parent : Control = null
  let clickevt = new Event<System.EventArgs>()
  let mousedownevt = new Event<MouseEventArgs>()
  let mousemoveevt = new Event<MouseEventArgs>()
  let mouseupevt = new Event<MouseEventArgs>()
  

  member this.CoordinateType
    with get() = coordinates
    and set(v) = coordinates <- v
 
  member this.Location
    with get() = location
    and set(v) = location <- v

  member this.Size
    with get() = size
    and set(v) = size <- v
  
  member this.Parent 
    with get() = parent
    and set(v) = parent <- v
  


  abstract OnMouseDown : MouseEventArgs -> unit
  default this.OnMouseDown e = mousedownevt.Trigger(e)

  abstract OnMouseMove : MouseEventArgs -> unit
  default this.OnMouseMove e = mousemoveevt.Trigger(e)

  abstract OnMouseUp : MouseEventArgs -> unit
  default this.OnMouseUp e = mouseupevt.Trigger(e)

  abstract OnPaint : PaintEventArgs -> unit
  default this.OnPaint e = ()
  member this.Invalidate() =
    if parent <> null then parent.Invalidate()

  abstract HitTest : PointF -> bool
  default this.HitTest p =
    (new RectangleF(location, size)).Contains(p)

type LWContainer() as this =
  inherit UserControl()

  let transformPoint (m:Drawing2D.Matrix) (p:PointF) =
    let pts = [| p |]
    m.TransformPoints(pts)
    pts.[0]

  let transform = W2V()

  let controls = ResizeArray<LWControl>()

  let scrollUp () =
    transform.Translate(0.f, 10.f)
    this.Invalidate()

  
  member this.LWControls with get() = controls

  override this.OnMouseDown e =
    let p = PointF(single e.X, single e.Y)
    let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
    match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
    | Some c -> c.OnMouseDown(e)
    | None -> 
      let pw = transformPoint transform.V2W p
      let controlsWorld = controls |> Seq.filter (fun c -> c.CoordinateType = World)
      match (controlsWorld |> Seq.tryFind(fun c -> c.HitTest pw)) with
      | Some c -> c.OnMouseDown(e)
      | None -> ()
   

  override this.OnMouseMove e =
    let p = PointF(single e.X, single e.Y)
    let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
    match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
    | Some c -> c.OnMouseMove(e)
    | None -> ()

  override this.OnMouseUp e =
    let p = PointF(single e.X, single e.Y)
    let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
    match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
    | Some c -> c.OnMouseUp(e)
    | None -> ()

  override this.OnPaint e =
    let g = e.Graphics

    let t = g.Transform

    g.Transform <- transform.W2V

    for idx in (controls.Count - 1) .. -1 .. 0 do
      let c = controls.[idx]
      if c.CoordinateType = World then
        c.OnPaint e
    
    g.Transform <- t

    for idx in (controls.Count - 1) .. -1 .. 0 do
      let c = controls.[idx]
      if c.CoordinateType = View then
        c.OnPaint e

  override this.OnKeyDown e =
    match e.KeyCode with
    | Keys.W -> scrollUp()
    | Keys.A -> 
      transform.Translate(10.f, 0.f)
      this.Invalidate()
    | Keys.S -> 
      transform.Translate(0.f, -10.f)
      this.Invalidate()
    | Keys.D ->
      transform.Translate(-10.f, 0.f)
      this.Invalidate()
    | Keys.Q ->
      transform.Rotate(10.f)
      this.Invalidate()
    | Keys.E ->
      transform.Rotate(-10.f)
      this.Invalidate()
    | Keys.Z ->
      transform.Scale(1.1f, 1.1f)
      this.Invalidate()
    | Keys.X ->
      transform.Scale(1.f/1.1f, 1.f/1.1f)
      this.Invalidate()
    | _ -> ()

type Rectbutton() as this =
  
  inherit LWControl()
  
  do this.Size <- SizeF(64.f, 32.f)
  let mutable text = ""
  let mutable selected = false

  let mutable font = Font("Arial", 8.f)
  let clickevt = new Event<System.EventArgs>()
  let mousedownevt = new Event<MouseEventArgs>()
  let mousemoveevt = new Event<MouseEventArgs>()
  let mouseupevt = new Event<MouseEventArgs>()

  member this.Click= clickevt.Publish
  member this.MouseDown = mousedownevt.Publish
  member this.MouseUp = mouseupevt.Publish
  member this.MouseMove = mousemoveevt.Publish
  member this.Text   
   with get() = text
   and set(v) = text <- v; this.Invalidate() 
  
  member this.Selected
   with get() = selected
   and set(v)= selected<-v;this.Invalidate()
  member this.Font
    with get() = font
    and set(v) = font<- v
 
  
  override this.OnMouseUp e = mouseupevt.Trigger(e); clickevt.Trigger(new System.EventArgs())
  
  override this.OnPaint e =
    let g = e.Graphics
    g.FillRectangle(Brushes.Gray , new Rectangle( PointFtoPoint(this.Location), SizeFtoSize(this.Size) ) );
    let sz = g.MeasureString(text, this.Parent.Font)
    g.DrawString(text, Font(this.Font.FontFamily, 8.f), Brushes.Black, PointF(((this.Size.Width - sz.Width) / 2.f)+this.Location.X, (this.Size.Height - sz.Height) / 2.f+this.Location.Y))
    if this.Selected then
      g.DrawRectangle(Pen(Color.Black, 2.f) , new Rectangle( PointFtoPoint(this.Location), SizeFtoSize(this.Size) ) );
    base.OnPaint(e)


type CirButtons() as this =
  inherit LWControl()
  
  do this.Size <- SizeF(32.f, 32.f)
  let mutable text = ""
  let mutable selected = false
  let mutable color = Color.Black
  
  let clickevt = new Event<System.EventArgs>()
  let mousedownevt = new Event<MouseEventArgs>()
  let mousemoveevt = new Event<MouseEventArgs>()
  let mouseupevt = new Event<MouseEventArgs>()

  member this.Click= clickevt.Publish
  member this.MouseDown = mousedownevt.Publish
  member this.MouseUp = mouseupevt.Publish
  member this.MouseMove = mousemoveevt.Publish
  member this.Text   
   with get() = text
   and set(v) = text <- v; this.Invalidate() 
  
  member this.Selected
   with get() = selected
   and set(v)= selected<-v;this.Invalidate()
  member this.Color
    with get() = color
    and set(v) = color<-v; this.Invalidate()
  
  
  override this.HitTest p =
    let radius = this.Size.Width/2.f
    let center = PointF( this.Location.X + (this.Size.Width/2.f), this.Location.Y + (this.Size.Height/2.f))
    let sqr v = v * v
    let x1, y1 = center.X - float32(p.X), center.Y - float32(p.Y)
    sqr x1 + sqr y1 <= sqr radius

  override this.OnMouseUp e = mouseupevt.Trigger(e); clickevt.Trigger(new System.EventArgs())
  
  override this.OnPaint e =
    let g = e.Graphics
    g.FillEllipse( new SolidBrush(this.Color), new Rectangle(PointFtoPoint(this.Location), SizeFtoSize(this.Size)))
    let sz = g.MeasureString(text, this.Parent.Font)
    g.DrawString(text, this.Parent.Font, Brushes.White, PointF(((this.Size.Width - sz.Width) / 2.f )+this.Location.X, ((this.Size.Height - sz.Height) / 2.f)+this.Location.Y))
    if this.Selected then
      g.DrawEllipse(Pen(this.Color),new Rectangle( int(this.Location.X)-4, int(this.Location.Y)-4, int(this.Size.Width)+8, int(this.Size.Height)+8)) 

