(* WebOnCont continuation passing style web framework *)
(* Web Shop example *)
(* @author Timur Abishev (abishev.timur@gmail.com) *)
(* GPL license http://www.gnu.org/licenses/gpl.html *)
module mw.example.shop

open mw.httpcontinuation
open mw.server

(* templates *)
let hrefToCont key text = 
  "<a href='?key="+key+"'>"+text+"</a>"

let htmlCodeFromText text =
  "<html><body>"+text+"</body></html>"

let authTemplate (key : string) = 
  htmlCodeFromText ("Hello World Auth! What is your name? <form><input type=\"hidden\" name=\"key\" value=\""+key+"\"/><input type = \"text\" name=\"name\" value=\"Name Here!\"/><input type=\"submit\"/></form>")

let mainTemplate (key : string) = 
  htmlCodeFromText ("It's online show! We are has: <a href='?id=1&key="+key+"'>book 1</a> and <a href='?id=2&key="+key+"'>book 2</a>")

let buyTemplate (id : string) (key : string) = 
  htmlCodeFromText ("Book "+id+" added to basket. Go to <a href='?action=basket&key="+key+"'>basket</a> or to <a href='?action=main&key="+key+"'>main</a>")

let basketTemplate (ids : string list) (key : string) = 
  htmlCodeFromText ("Your books: "+(sprintf "%A" ids)+". Go to <a href='?key="+key+"'>main</a>")

(* handlers *)
let mutable authNames = Map [("Session", "Name")]
let mutable books = Map [("Session", ["1";"2"])]

let auth = httpContinuation {
  let! session = getSession
  match authNames.ContainsKey session with
  | true -> return authNames.[session]
  | false -> 
    let! result = request authTemplate
    let name = result.QueryString.["name"]
    authNames <- authNames.Add (session, name);
    books <- books.Add(session, [])
    return name
} 

let getIdsInBasket = httpContinuation {
  let! session = getSession
  return books.[session]
}

let addIdToBasket id = httpContinuation {
  let! session = getSession
  books <- books.Add(session, (id :: books.[session]))
  return ()
}

let printBasket afterCont = httpContinuation {
  let! idsInBasket = getIdsInBasket
  let! result = request (basketTemplate idsInBasket) 
  let! nextResult = afterCont
  return ()
}

let rec main = httpContinuation {
  let! result = request mainTemplate
  let idToBuy = result.QueryString.["id"]
  let! name = auth
  let! buyBook = (addIdToBasket idToBuy)
  let! nextGoResult = request (buyTemplate idToBuy)
  let nextAction = nextGoResult.QueryString.["action"]
  let! nextResult = (if nextAction = "basket" then printBasket main else main)
  return ()
}

mw.server.startServer "http://localhost:7777/mw/" main
