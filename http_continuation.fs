(* WebOnCont continuation passing style web framework *)
(* Http Continuation module *)
(* @author Timur Abishev (abishev.timur@gmail.com) *)
(* GPL license http://www.gnu.org/licenses/gpl.html *)
module mw.httpcontinuation
open System.Net

(* userFeedback - is feedback for user. = HttpListenerResponse and key *)
type UserFeedback = HttpListenerResponse*string
type 'a HttpContinuation = (UserFeedback*('a*UserFeedback -> unit)) -> unit

let ret (a : 'a) : 'a HttpContinuation = (fun (feedback, f) -> (f (a, feedback)); ())
let bind (mA : 'a HttpContinuation) (f : ('a -> 'b HttpContinuation)) : 'b HttpContinuation = 
  fun ((feedback, callback : ('b*UserFeedback -> unit))) -> 
    (mA (feedback, (fun (a,newFeedback) -> (f a (newFeedback, callback)))))

type HttpContinuationBuilder() =
  member b.Return(x) = ret x
  member b.Bind(mA,b2) = bind mA b2
    
let httpContinuation = new HttpContinuationBuilder()

