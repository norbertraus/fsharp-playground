module bounded_queue

type Agent<'T> = MailboxProcessor<'T>

#region "WebCrawler"

open System.Text.RegularExpressions
open System.Threading
open System.Net
open System.IO

let limit = 50
let linkPattern = "href=\s*\"[^\"h]*(http://[^&\"]*)\""
let getLinks (txt:string) =
  [ for m in Regex.Matches(txt, linkPattern) -> m.Groups.Item(1).Value ]

type RequestGate(n:int) =
  let semaphore = new Semaphore(initialCount=n, maximumCount=n)
  member x.AsyncAcquire(?timeout) =
     async { let! ok = Async.AwaitWaitHandle(semaphore, ?millisecondsTimeout=timeout)
            if ok then
              return { new System.IDisposable with
                          member x.Dispose() = semaphore.Release() |> ignore }
            else
              return! failwith "couldn't acquire a semaphore" }

let webRequestGate = RequestGate(5)

//fetch the URL and post the result to the urlCollector
let collectLinks (url:string) =
  async { //as async web request with the global state
          let! html =
            async { 
              //acquire an entry to the webRequestGate. Release it once out of scope
              use! holder = webRequestGate.AsyncAcquire()
              
              let req = WebRequest.Create(url, Timeout = 5)

              //wait for the WebResponse
              use! response = req.AsyncGetResponse()

              //get response stream
              use reader = new StreamReader(response.GetResponseStream())

              //read the response stream (syn once)
              return reader.ReadToEnd() }

          //compute the links synchronously
          let links = getLinks html

          //report synchronously
          do printfn "finished reading %s, got %d links" url (List.length links)

          return links }

//urlCollector is a single agnet that receives URLs as messages. It creates new async
//tasks that post messages back to this object

let urlCollector = 
  MailboxProcessor.Start(fun self ->
    
    //this is the main state of the urlCollector
    let rec waitForUrl (visited: Set<string>) =
      async { 
        //check the limit
        if visited.Count < limit then
          //wait for the URL...
          let! url = self.Receive()
          if not (visited.Contains(url)) then
            //create new task for the new url. Each collects 
            //links and posts the back to the urlCollector.
            do! Async.StartChild (async { let! links = collectLinks url
                      for link in links do self.Post link }) |> Async.Ignore
          return! waitForUrl(visited.Add(url)) }

    waitForUrl(Set.empty))
   
#endregion
  
#region "Play"
//let counter1 = 
//  new Agent<_>(fun inbox -> 
//    let rec loop n = 
//      async { printfn "n = %d, waiting..." n
//              let! msg = inbox.Receive()
//              return! loop(n + msg)}
//    loop 0)
//
//
//type internal msg =
//  | Increment of int
//  | Fetch of AsyncReplyChannel<int>
//  | Stop
//
//type CountingAgent() =
//  let counter = Agent.Start(fun inbox ->
//    //represents states from the state machine of the Agent 
//    let rec loop n = 
//      async { let! msg = inbox.Receive()
//              match msg with
//              | Increment i -> return! loop(n + i)
//              | Stop -> return ()
//              | Fetch replyChannel ->
//                //post response to replyChannel and cont
//                do replyChannel.Reply n
//                return! loop n }
//    loop 0)
//
//  member this.Increment(n) = counter.Post(Increment n)
//  member this.Stop() = counter.Post Stop 
//  member this.Fetch() = counter.PostAndReply(fun channel -> Fetch channel)
//
//let cnt = new CountingAgent()
//cnt.Increment(1)
//let v = cnt.Fetch()
//cnt.Increment(2)
////let v2 = cnt.Fetch()
//cnt.Stop()
//
//type Message =
//  | Message1
//  | Message2 of int
//  | Message3 of string
//
//let agent = 
//  Agent.Start(fun inbox ->
//    let rec loop() =
//      inbox.Scan(function
//        | Message1 -> 
//          Some(async { do printfn "message1!"
//                       return! loop() })
//        | Message2 n -> 
//          Some(async { do printfn "message2!"
//                       return! loop() })
//        | Message3 _ -> None) 
//    loop())
#endregion
