module D = Dom
module F = Froc
module Fd = Froc_dom
module Fda = Froc_dom_anim

let (>>=) = F.(>>=)

let (|>) x f = f x

class type console =
object
  method log : _ -> unit
end

let console = (Ocamljs.var "console" : console)

class type touch =
object
  method _get_clientX : int
  method _get_clientY : int
  method identifier : int
  method _get_pageX : int
  method _get_pageY : int
  method _get_screenX : int
  method _get_screenY : int
  method _get_target : < .. >
end

class type touchEvent =
object
  inherit Dom.uIEvent

  method _get_changedTouches : touch array
  method _get_targetTouches : touch array
  method _get_touches : touch array
end

let touchEvent name (elem : #Dom.element) =
  let e, s = F.make_event () in
  let f = F.send s in
  elem#addEventListener name f false;
  F.cleanup (fun () -> elem#removeEventListener name f false);
  (Obj.magic e : touchEvent F.event)

type piece = {
  id : int;
  w : int;
  h : int;
  color : Fda.color;
  xy : (int * int) F.behavior;
  angle : float F.behavior;
}

let onload () =
  let canvas = (D.document#getElementById "canvas" : D.canvas) in
  let click_canvas = (D.document#createElement "canvas" : D.canvas) in

  let xy_of_drag e init =
    e |>
    F.map
      (fun e ->
         match e#_get_type with
           | "mousedown" when not e#_get_shiftKey ->
               Fd.mouseEvent "mousemove" canvas |>
               F.collect_e
                 (fun ((x, y), _) e ->
                    let x' = e#_get_clientX and y' = e#_get_clientY in
                    ((x', y'), (x' - x, y' - y)))
                 ((e#_get_clientX, e#_get_clientY), (0, 0)) |>
               F.map (fun (_, d) -> d)
           | _ -> F.never) |>
    F.join_e |>
    F.collect_b (fun (x, y) (dx, dy) -> x + dx, y + dy) init in

  let size = Fd.window_innerSize_b () in
  F.notify_b size begin fun (w, h) ->
    let w = w - 2 * canvas#_get_offsetLeft in
    let h = h - 2 * canvas#_get_offsetTop in
    canvas#_set_width w;
    canvas#_set_height h;
    click_canvas#_set_width w;
    click_canvas#_set_height h;
  end;

  let num_shapes = 25 in

  (* shape 0 is the background *)
  let shape_events = Array.init (num_shapes + 1) (fun _ -> F.make_event ()) in

  let mouse_events =
    F.merge
      (List.map
         (fun t -> Fd.mouseEvent t canvas)
         [ "mousedown"; "mouseup"; "mouseout" ]) in

  (* dispatch mousedowns to the right shape. this is rather imperative. *)
  let last = ref 0 in
  F.notify_e mouse_events begin fun e ->
    match e#_get_type with
      | "mousedown" ->
          let i =
            let x = e#_get_clientX - canvas#_get_offsetLeft in
            let y = e#_get_clientY - canvas#_get_offsetTop in
            let id = (click_canvas#getContext "2d")#getImageData (float_of_int x) (float_of_int y) 1. 1. in
            let d = id#_get_data in
            d.(0) in
          let (_, s) = shape_events.(i) in
          F.send s e;
          last := i
      | _ ->
          let (_, s) = shape_events.(!last) in
          F.send s e;
          last := 0
  end;

  let shapes =
    Array.init
      num_shapes
      (fun i ->
         let (e, _) = shape_events.(i + 1) in

         let xy = xy_of_drag e (Random.int 1024, Random.int 512) in

         let angle =
           e |>
           F.map
             (fun e ->
                match e#_get_type with
                  | "mousedown" when e#_get_shiftKey ->
                      let (cx, cy) = F.sample xy in
                      let x = e#_get_clientX and y = e#_get_clientY in
                      let a = atan2 (float_of_int (x - cx)) (float_of_int (y - cy)) in
                      Fd.mouseEvent "mousemove" canvas |>
                      F.collect_e
                        (fun (td, _) e ->
                           let x = e#_get_clientX and y = e#_get_clientY in
                           let a = atan2 (float_of_int (x - cx)) (float_of_int (y - cy)) in
                           let d = a -. td in
                           (td +. d, d))
                        (a, 0.) |>
                      F.map (fun (_, d) -> d)
                  | _ -> F.never) |>
           F.join_e |>
           F.collect_b (fun a d -> a +. d) 0. in

         {
           id = i + 1;
           w = Random.int 128;
           h = Random.int 128;
           color = Fda.color ~a:(Random.int 256) (Random.int 256) (Random.int 256) (Random.int 256);
           xy = xy;
           angle = angle;
         }) in

  let xy =
    let (e, _) = shape_events.(0) in
    xy_of_drag e (0, 0) in

  (* must depend on size since setting height/width clears canvas
     XXX but doesn't work if shapes doesn't change *)

  let shapes color = F.bind xy begin fun (px, py) ->
    F.bindN
      (List.map
         (fun s ->
            F.bind2 s.xy s.angle begin fun (x, y) a ->
              F.return
                (fun ctx ->
                   ctx#translate (float_of_int (px + x)) (float_of_int (py + y));
                   ctx#rotate (-. a);
                   ctx#translate (float_of_int (-px -x)) (float_of_int (-py -y));
                   Fda.fillRect (float_of_int (px + x - s.w/2), float_of_int (py + y - s.h/2)) (float_of_int s.w) (float_of_int s.h) (color s) ctx)
            end)
         (Array.to_list shapes))
      F.return
  end in
  Fda.attach canvas (shapes (fun s -> s.color));
  Fda.attach click_canvas (shapes (fun s -> Fda.color s.id 0 0));

;;

D.window#_set_onload onload
