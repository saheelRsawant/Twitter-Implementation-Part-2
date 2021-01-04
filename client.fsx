// #r "nuget: Akka.FSharp" 
#r "nuget: FSharp.Data, Version=3.0.1"
// open Akka.FSharp
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open System
/// User registration command
let registerUser userName =
    let sendingJson = sprintf """{"userId": "%s","status": "false"}""" userName
    let response = Http.Request(
                    "http://localhost:5000/api/registerUser",
                    httpMethod = "POST",
                    headers = [ ContentType HttpContentTypes.Json ],
                    body = TextRequest sendingJson
    )
    let r1 = response.Body
    let response1 =
        match r1 with
        | Text responseJson -> responseJson
        | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b

    
    let responseJson = FSharp.Data.JsonValue.Parse(response1)
    let returnedJson = responseJson.GetProperty("resp")
    
    returnedJson




/// Function to Login a user
/// 
let loginUser userName =
    let sendingJson = sprintf """{"userId": "%s"}""" userName
    let response = Http.Request(
                    "http://localhost:5000/api/loginUser",
                    httpMethod = "POST",
                    headers = [ ContentType HttpContentTypes.Json ],
                    body = TextRequest sendingJson
    )
    let r1 = response.Body
    let response1 =
        match r1 with
        | Text responseJson -> responseJson
        | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
    let responseJson = FSharp.Data.JsonValue.Parse(response1)
    let returnedJson = responseJson.GetProperty("resp")
    
    returnedJson



let logoutUser userName =
    
    let sendingJson = sprintf """{"userId": "%s"}""" userName
    let response = Http.Request(
                    "http://localhost:5000/api/logoutUser",
                    httpMethod = "POST",
                    headers = [ ContentType HttpContentTypes.Json ],
                    body = TextRequest sendingJson
    )
    let r1 = response.Body
    let response1 =
        match r1 with
        | Text responseJson -> responseJson
        | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b


    let responseJson = FSharp.Data.JsonValue.Parse(response1)
    let returnedJson = responseJson.GetProperty("resp")
    
    returnedJson




let postTweet (userName:string, tweet:string ) =
    let sendingJson = sprintf """{"userId": "%s", "tweet": "%s"}""" userName tweet
    let response = Http.Request(
                    "http://localhost:5000/api/tweet",
                    httpMethod = "POST",
                    headers = [ ContentType HttpContentTypes.Json ],
                    body = TextRequest sendingJson
    )
    let r1 = response.Body
    let response1 =
        match r1 with
        | Text responseJson -> responseJson
        | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
    let responseJson = FSharp.Data.JsonValue.Parse(response1)
    let returnedJson = responseJson.GetProperty("resp")
    
    returnedJson


let followUser (userName1:string, username2:string ) =
    let sendingJson = sprintf """{"userId1": "%s", "userId2": "%s"}""" userName1 username2
    let response = Http.Request(
                    "http://localhost:5000/api/follow",
                    httpMethod = "POST",
                    headers = [ ContentType HttpContentTypes.Json ],
                    body = TextRequest sendingJson
    )
    let r1 = response.Body
    let response1 =
        match r1 with
        | Text responseJson -> responseJson
        | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
    let responseJson = FSharp.Data.JsonValue.Parse(response1)
    let returnedJson = responseJson.GetProperty("resp")
    
    returnedJson



let getTweetsWithHashTag hashtagToRequest =
    try
        let url = "http://localhost:5000/api/tweets/hashtag/"+hashtagToRequest
        let responseJson = FSharp.Data.JsonValue.Load url
        let foundResponses = responseJson.GetProperty("foundTweets")
        let mutable len=0
        for i in foundResponses do
            len<-len+1
        if len > 0 then
            printfn "Tweets with the #%s found: " hashtagToRequest
            for i in foundResponses do
                printfn "%A" i
            printfn ""
            
    with
    | _ -> printfn "No tweets with the #%s found" hashtagToRequest

let getTweetsWithMentionTag mentionTagtoRequest =
    try
        let url = "http://localhost:5000/api/tweets/mentiontag/"+mentionTagtoRequest
        let responseJson = FSharp.Data.JsonValue.Load url
        let foundResponses = responseJson.GetProperty("foundTweets")
        let mutable len=0
        for i in foundResponses do
            len<-len+1
        if len > 0 then
            printfn "Tweets with the @%s found: " mentionTagtoRequest
            for i in foundResponses do
                printfn "%A" i
            printfn ""
            
            
    with
    | _ -> printfn "No tweets with the @%s found" mentionTagtoRequest


let getAllLiveTweets() =
    try
        let url = "http://localhost:5000/api/tweets"
        let responseJson = FSharp.Data.JsonValue.Load url
        let foundResponses = responseJson.GetProperty("foundTweets")
        let mutable len=0
        for i in foundResponses do
            len<-len+1
        if len > 0 then
            printfn "All live tweets are :"
            for i in foundResponses do
                printfn "%A" i
            
    with
    | _ -> printfn "No tweets with the found. "


let getSubscribedTweets(username:string) = 
    let sendingJson = sprintf """{"userId": "%s"}""" username 
    let response = Http.Request(
                    "http://localhost:5000/api/subscribe",
                    httpMethod = "POST",
                    headers = [ ContentType HttpContentTypes.Json ],
                    body = TextRequest sendingJson
    )
    let r1 = response.Body
    let response1 =
        match r1 with
        | Text responseJson -> responseJson
        | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
    try

        let responseJson = FSharp.Data.JsonValue.Parse(response1)
        let foundResponses = responseJson.GetProperty("foundTweets")
        let mutable len=0
        for i in foundResponses do
            len<-len+1
        if len > 0 then
            printfn "Your subsribed tweets are:"
            let mutable counter = 1
            for i in foundResponses do
                printfn "%i %A" counter i 
                counter <- counter + 1
            printfn ""    
        
            
    with
    | _ -> printfn "No subscribed tweets for %s found." username


let getRetweetwithId(tweetId:string, username:string) = 
    try
        let url = "http://localhost:5000/api/retweets/"+tweetId
        let responseJson = FSharp.Data.JsonValue.Load url
        let foundResponses = responseJson.GetProperty("resp")
        printfn "%s ReTweeted: %A\n" username foundResponses

    with
    | _ -> printfn "No retweets found. "


let postRetweet(tweetId: string, username: string) = 
    let sendingJson = sprintf """{"tweetId": "%s", "username": "%s"}""" tweetId username
    let response = Http.Request(
                    "http://localhost:5000/api/postRetweets",
                    httpMethod = "POST",
                    headers = [ ContentType HttpContentTypes.Json ],
                    body = TextRequest sendingJson
    )
    let r1 = response.Body
    let response1 =
        match r1 with
        | Text responseJson -> responseJson
        | Binary b -> System.Text.ASCIIEncoding.ASCII.GetString b
    ()


let mutable flag = true
let mutable tweetFlag = true
let mutable followFlag = true
let mutable reTweetFlag = true


while flag do  
    printfn "1.Register User\t2.Login User\t3.Logout User\t4.Send Tweet\t5.Follow User\t6.Get Subscribed Tweets\t7.Query Tweets\t8.Get All Live Tweets\t9.Program Termination"
    printfn "Enter your input choice: "
    let input = System.Console.ReadLine()
    match input with 

    | "1" -> // Register
        printfn "Enter username to register: "
        let username = System.Console.ReadLine()
        let response = registerUser username
        printfn "%A\n" response

    | "2" -> // Login User
        printfn "Enter username to login: "
        let username = System.Console.ReadLine()
        let response = loginUser username
        printfn "%A\n" response
        
    | "3" ->  // Logout
        printfn "Enter username to Logout: "
        let username = System.Console.ReadLine()
        let response = logoutUser username
        printfn "%A\n" response


    | "4" -> // Tweet
        tweetFlag <- true
        printfn "Enter the username: "
        let username = System.Console.ReadLine()

        while tweetFlag do
            printfn "Post new tweet?: (Y/n) "
            let input =  System.Console.ReadLine()
            match input with 
            | "Y" ->
                printfn "Enter new tweet: "
              
                let tweet = System.Console.ReadLine()
                let response = postTweet(username, tweet)
                printfn "%A\n" (response)
           
            | "n"->
                tweetFlag <- false
            | _-> printfn "Invalid Input"


    | "5"-> // Follow
        followFlag <- true
        printfn "Enter your username: "
        let input1 = System.Console.ReadLine()
        printfn "Enter username you want to follow: "
        let input2 = System.Console.ReadLine()

        let response = followUser(input1, input2)
        printfn "%s" (string(response))

        while followFlag do
            printfn "Follow more users? (Y/n)"
            let input3 = Console.ReadLine()
            match input3 with
                | "Y" ->
                    printfn "Enter username you want to follow: "
                    let input4 = System.Console.ReadLine()
                    let response = followUser(input1, input4)
                    printfn "%s\n" (string(response))
                    
                | "n" ->
                    followFlag <- false
                | _-> printfn "Invalid Input"
            ()

    | "6" -> // Subscribe
        printfn "Enter your username: "
        let input = System.Console.ReadLine()
        getSubscribedTweets(input)
        reTweetFlag <- true 
        while reTweetFlag do
            printfn "Retweet any tweets? (Y/n)"
            let wantToRetweet = System.Console.ReadLine()
            match wantToRetweet with 
            | "Y" ->

                printfn "Enter Tweet ID to Retweet: "
                let tweetNo = System.Console.ReadLine() 

                getRetweetwithId(tweetNo,input)
                postRetweet(tweetNo, input)
               

            | "n" ->
                reTweetFlag <- false
            | _ -> printfn "Invalid command"
        

    | "7"-> // Search query
        printfn "Search Query: 1.HashTag 2. MentionTag"
        let input = System.Console.ReadLine()
        match input with 
        | "1" ->
            printfn "Enter HashTag:"
            let word = System.Console.ReadLine()
            getTweetsWithHashTag word
         
        |"2" ->
            printfn "Enter MentionTag:"
            let word = System.Console.ReadLine()
            getTweetsWithMentionTag word
            
        | _-> printfn "Invalid Input"

    | "8" -> // Live Tweets
        getAllLiveTweets()

    | "9" -> // Terminate
        flag<-false
        
    | _-> printfn "Invalid Case"
