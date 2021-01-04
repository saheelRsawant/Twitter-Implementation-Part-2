module PeopleApi.App

open System
open System.Collections.Generic
open WebSharper
open WebSharper.Sitelets

/// The types used by this application.
module Model =

    /// Data about a person. Used both for storage and JSON parsing/writing.
    /// Data type for registering a user
    type UserData = 
        { 
            userId: string
            status : string     
        }

    type LoginData = 
        {
            userId : string
        }
    
    type TweetData = 
        {
            userId : string
            tweet : string
        } 

    type FollowData = 
        {
            userId1 : string
            userId2 : string
        } 


    type HashtagMentiontag = 
        {
            hashtag : string
            mentiontag : string
        } 
    
    type SubscribeData = 
        {
            userId : string
        }

    type QueryByHashTag = 
        {
            word : string
        }


    type PostRetweetData = 
        {
            tweetId : string
            username : string
        }

    /// The type of REST API endpoints.
    /// This defines the set of requests accepted by our API.
    type ApiEndPoint =
        /// Accepts POST requests to /registerUser UserData as JSON body
        | [<EndPoint "POST /registerUser"; Json "userData">] CreateUser of userData: UserData
        | [<EndPoint "POST /loginUser"; Json "loginData">] LoginUser of loginData: LoginData
        | [<EndPoint "POST /logoutUser"; Json "loginData">] LogoutUser of loginData: LoginData
        | [<EndPoint "POST /tweet"; Json "tweetData">] PostTweet of tweetData: TweetData
        | [<EndPoint "GET /tweets">] GetTweets
        | [<EndPoint "POST /follow"; Json "followData">] PostFollow of followData: FollowData
        | [<EndPoint "POST /subscribe"; Json "subscribeData">] PostSubscribe of subscribeData: SubscribeData
        | [<EndPoint "GET /tweets/hashtag">] GetHashtag of word: string
        | [<EndPoint "GET /tweets/mentiontag">] GetMentiontag of word: string
        | [<EndPoint "GET /retweets">] GetRetweet of id: string
        | [<EndPoint "POST /postRetweets"; Json "postRetweets">] PostRetweets of postRetweets: PostRetweetData

    /// The type of all endpoints for the application.
    type EndPoint =
        
        /// Accepts requests to /
        | [<EndPoint "/">] Home

        /// Accepts requests to /api/...
        | [<EndPoint "/api">] Api of Cors<ApiEndPoint>

    /// Error result value.
    type Error = { error : string }

    /// Alias representing the success or failure of an operation.
    /// The Ok case contains a success value to return as JSON.
    /// The Error case contains an HTTP status and a JSON error to return.
    type ApiResult<'T> = Result<'T, Http.Status * Error>

  
    type Resp = { resp: string }

  
    type HashTagResponse= 
        {
            foundTweets: Set<string>
        }



    type TweetResp = 
        {
            userId: string 
            tweet: string
        }

     type GetAllTweets = 
        {
           foundTweets : list<string>
        }




   

    

open Model

/// This module implements the back-end of the application.
/// It's a CRUD application maintaining a basic in-memory database of people.
module Backend =
    
    let mutable allUsers : Set<string> = Set.empty
    let mutable setOfLoggedInUsers : Set<string> = Set.empty
    
    let mutable totalUsers:int = 0 
    let mutable totalTweets:int = 0 
    let mutable loggedInUsers: int = 0
    let mutable loggedOutUsers: int = 0
    let mutable totalReTweets: int = 0



////////////////////////////

    let mutable mapTweetIdToUserName : Map<String, String> = Map.empty 
    let mutable mapUserToHashTags : Map<String, Set<string>> = Map.empty
    let mutable mapUserToMentionTags : Map<String, Set<string>> = Map.empty 
    let mutable mapUserNametoTweets: Map<String, list<string>> = Map.empty
    let mutable allUsersMap : Map<string, bool> = Map.empty
    let mutable followersMap : Map<String, list<string>> = Map.empty
    let mutable followingMap : Map<String, list<string>> = Map.empty    
    let mutable finalStr : list<string> = List.empty  


    ///////////////////////////////////// 
    /// 
    /// 

    

    

    let strToBool(str: string) = 
        let final = Boolean.Parse(str)
        final

    let CreateUser (data: UserData): ApiResult<Resp> =
        if allUsers.Contains(data.userId) then
            let err = sprintf "User already registered."
            Ok { resp = err }
        else 
            allUsers <- allUsers.Add(data.userId)
            let status = strToBool(data.status)
            totalUsers <- totalUsers + 1
            loggedOutUsers <- loggedOutUsers + 1
            allUsersMap <- allUsersMap.Add(data.userId, status)
            followingMap <- followingMap.Add(data.userId, List.empty)
            followersMap <- followersMap.Add(data.userId, List.empty)
            printfn "\nLOG: User %A registered." data.userId
            printfn "\nLOG: Total Registered Users: %i\nTotal Online Users: %i\nTotal Offline Users: %i\nTotal Tweets: %i\nTotal ReTweets: %i\n" totalUsers loggedInUsers loggedOutUsers totalTweets totalReTweets 
            Ok { resp = data.userId + " registered."}




    let LoginUser(data:LoginData) : ApiResult<Resp> =         
        if allUsers.Contains(data.userId) then
            if not(setOfLoggedInUsers.Contains(data.userId)) then
                setOfLoggedInUsers <-setOfLoggedInUsers.Add(data.userId)
                let status = allUsersMap.TryFind(data.userId)
                match status with 
                | Some x ->
                    let finalStatus = not(x)
                    allUsersMap <- allUsersMap.Add(data.userId, finalStatus)
                    loggedInUsers <- loggedInUsers + 1
                    loggedOutUsers <- loggedOutUsers - 1
                    printfn "\nLOG: User %A Logged in" data.userId
                    printfn "\nLOG: Total Registered Users: %i\nTotal Online Users: %i\nTotal Offline Users: %i\nTotal Tweets: %i\nTotal ReTweets: %i\n" totalUsers loggedInUsers loggedOutUsers totalTweets totalReTweets 
                    Ok { resp = data.userId + " logged in." }
                | None ->
                    let err = sprintf "Key does not exist"                   
                    Ok { resp = err }
            else 
                let err = sprintf "User already logged in."         
                Ok { resp = err }
        else
            let err = sprintf "User not registered. Please Register."
            Ok { resp = err }

           


    let LogoutUser(data:LoginData) : ApiResult<Resp> = 
        printfn "\nLOG: User %A attempting to logout" data.userId   
        if allUsers.Contains(data.userId) then
            if (setOfLoggedInUsers.Contains(data.userId)) then
                let status = allUsersMap.TryFind(data.userId)
                match status with 
                | Some x ->
                    let finalStatus = not(x)
                    allUsersMap <- allUsersMap.Add(data.userId, finalStatus)
                    loggedInUsers <- loggedInUsers - 1
                    loggedOutUsers <- loggedOutUsers + 1
                    printfn "\nLOG: User %A Logged out" data.userId
                    printfn "\nLOG: Total Registered Users: %i\nTotal Online Users: %i\nTotal Offline Users: %i\nTotal Tweets: %i\nTotal ReTweets: %i\n" totalUsers loggedInUsers loggedOutUsers totalTweets totalReTweets 
                    Ok { resp = data.userId + " logged out succesfully."}
                | None ->
                    let err = sprintf "Key does not exist"
                    Ok { resp = err }           
            else 
                let err = sprintf "User not logged in."
                Ok { resp = err }            
        else
            let err = sprintf "User not registered. Please Register."
            Ok { resp = err }

                    

    let splitLine = (fun (line:string)->Seq.toList(line.Split " "))




    let PostTweet(data:TweetData) : ApiResult<Resp> = 
        let username = data.userId
        let tweet = data.tweet 
        if allUsers.Contains(username) then
            if setOfLoggedInUsers.Contains(username) then
                if mapUserNametoTweets.ContainsKey(username) then
                    let mutable temp = mapUserNametoTweets.Item(username)
                    temp <- temp @ [tweet]
                    mapUserNametoTweets <- mapUserNametoTweets.Add(username, temp)
                else 
                    mapUserNametoTweets <- mapUserNametoTweets.Add(username, list.Empty)
                    let mutable temp = mapUserNametoTweets.Item(username)
                    temp <- temp @[tweet]
                    mapUserNametoTweets <- mapUserNametoTweets.Add(username, temp)

                let res = splitLine tweet
                for value in res do
                    if value.Contains("#") then                
                        if mapUserToHashTags.ContainsKey(value.[1..value.Length-1]) then
                            mapUserToHashTags <- mapUserToHashTags.Add(value.[1..value.Length-1], mapUserToHashTags.[value.[1..value.Length-1]].Add(tweet))
                        else 
                            mapUserToHashTags <- mapUserToHashTags.Add(value.[1..value.Length-1], Set.empty)
                            mapUserToHashTags <- mapUserToHashTags.Add(value.[1..value.Length-1], mapUserToHashTags.[value.[1..value.Length-1]].Add(tweet))
                    if value.Contains("@") then
                        if mapUserToMentionTags.ContainsKey(value.[1..value.Length-1]) then
                            mapUserToMentionTags <- mapUserToMentionTags.Add(value.[1..value.Length-1], mapUserToMentionTags.[value.[1..value.Length-1]].Add(tweet))
                        else 
                            mapUserToMentionTags <- mapUserToMentionTags.Add(value.[1..value.Length-1], Set.empty)
                            
                            mapUserToMentionTags <- mapUserToMentionTags.Add(value.[1..value.Length-1], mapUserToMentionTags.[value.[1..value.Length-1]].Add(tweet))
                printfn "\nLOG: Total Registered Users: %i\nTotal Online Users: %i\nTotal Offline Users: %i\nTotal Tweets: %i\nTotal ReTweets: %i\n" totalUsers loggedInUsers loggedOutUsers totalTweets totalReTweets
                printfn "\nLOG: User to Tweets: %A" mapUserNametoTweets
                printfn "\nLOG: User to Hastags: %A" mapUserToHashTags
                printfn "\nLOG: User to Mentions: %A" mapUserToMentionTags
                printfn "\nLOG: User %s Tweeted:  \"%s\" " username tweet             
                totalTweets<- totalTweets + 1
                let str = sprintf "User %s Tweeted: %s" username tweet
                Ok{resp = str}
            else 
                let err = sprintf "User not logged in."
                Ok { resp = err }         
        else 
            let err = sprintf "User is not registered."
            Ok { resp = err }
            
            

           

    
               
    let PostFollow (data: FollowData) : ApiResult<Resp> = 
        let wantsToFollow = data.userId1
        let isFollowedBy = data.userId2
        if isFollowedBy = wantsToFollow then
            let err = sprintf "User cannot follow itself."
            Ok { resp = err }
        elif allUsers.Contains(wantsToFollow) then
            if allUsers.Contains(isFollowedBy) then
                if setOfLoggedInUsers.Contains(wantsToFollow) then                  
                    let mutable temp = followersMap.Item(isFollowedBy)
                    let mutable alreadyFollowing = false
                    for user in temp do
                        if user = wantsToFollow then
                            alreadyFollowing <- true
                    let mutable temp1 = followingMap.Item(wantsToFollow)                    
                    if alreadyFollowing then
                        let err = sprintf "%s already follows %s." wantsToFollow isFollowedBy
                        Ok { resp = err }
                    else
                        temp <- temp @ [wantsToFollow]
                        followersMap <- followersMap.Add(isFollowedBy, temp)
                        temp1<-temp1 @ [isFollowedBy]
                        followingMap <- followingMap.Add(wantsToFollow, temp1)
                        printfn "\nLOG: User %s started following User %s" wantsToFollow isFollowedBy
                        let finalStr = sprintf "User %s started following %s" wantsToFollow isFollowedBy
                        printfn "\nLOG: Following Map: %A" followingMap
                        printfn "\nLOG: Followers Map: %A" followersMap
                        Ok{resp = finalStr}
                else
                    let err = sprintf "%s please login to follow %s" wantsToFollow isFollowedBy
                    Ok { resp = err }               
            else 
                let err = sprintf "%s named user does not exist" isFollowedBy
                Ok { resp = err }
        else 
            let err = sprintf "%s is not registered. Please Register" wantsToFollow
            Ok { resp = err }



    let PostSubscribe(data:SubscribeData) : ApiResult<GetAllTweets> = 
        let username = data.userId
        let mutable followersSet : list<string> = List.empty
        if allUsers.Contains(username) then
            if setOfLoggedInUsers.Contains(username) then
                let tempList = followingMap.TryFind(username)
                match tempList with
                | Some x-> 
                    if x.Length = 0 then
                        let err = sprintf "%s must follow first to get subscribed tweets" username
                        Ok { foundTweets = [err] }
                    else 
                        followersSet<- followersSet @ followingMap.Item(username)                       
                        for follower in [0..followersSet.Length-1] do
                            let mutable sendStr : list<string> = List.Empty
                            if(mapUserNametoTweets.ContainsKey(followersSet.[follower])) then
                                let mutable temp = mapUserNametoTweets.Item(followersSet.[follower])
                                for tweet in [0..temp.Length-1] do
                                    sendStr <- sendStr @ [ temp.[tweet] ]                          
                            finalStr <- finalStr @ sendStr                            
                        Ok{foundTweets = finalStr}
                | None -> 
                    let mutable abc : list<string> = List.empty  
                    Ok{foundTweets = abc}                           
            else
                let err = sprintf "%s is not logged in. Please Login" username
                Ok { foundTweets = [err] }
        else
            let err = sprintf "%s is not registered. Please Register" username
            Ok { foundTweets = [err] }


    let hashtagNotFound() : ApiResult<'T> =
        Error (Http.Status.NotFound, { error = "HashTag not found." })


    let mentiontagNotFound() : ApiResult<'T> =
        Error (Http.Status.NotFound, { error = "HashTag not found." })

    let GetTweets () : ApiResult<GetAllTweets> =
        let mutable finalTweets : list<string> = List.Empty
        for KeyValue(_, tweet) in mapUserNametoTweets do
            finalTweets<- finalTweets @ tweet
        printfn "\nLOG: Total Registered Users: %i\nTotal Online Users: %i\nTotal Offline Users: %i\nTotal Tweets: %i\nTotal ReTweets: %i\n" totalUsers loggedInUsers loggedOutUsers totalTweets totalReTweets 
        Ok{foundTweets = finalTweets}

    

    let GetHashtag (word: string) : ApiResult<HashTagResponse> =
        lock mapUserToHashTags <| fun () ->
        match mapUserToHashTags.TryGetValue(word) with
        | true, tweets -> Ok {foundTweets = tweets}
        | false, _ -> hashtagNotFound()

     
   



    let GetMentiontag (word: string) : ApiResult<HashTagResponse> =
        lock mapUserToMentionTags <| fun () ->
        match mapUserToMentionTags.TryGetValue(word) with
        | true, tweets -> Ok {foundTweets = tweets}  
        | false, _ -> mentiontagNotFound()

   
    let GetRetweet(id:string) : ApiResult<Resp> = 
        let finalId = id |> int
        let str = finalStr.[finalId-1]
        Ok{resp = str}



    let PostRetweets (word:PostRetweetData) : ApiResult<Resp> =
        let tweetId = word.tweetId |> int
        let username = word.username
        let mutable retweet : string = "RE: " + finalStr.[tweetId-1]
        if mapUserNametoTweets.ContainsKey(username) then
            let mutable temp = mapUserNametoTweets.Item(username)
            temp <- temp @ [retweet]
            mapUserNametoTweets <- mapUserNametoTweets.Add(username, temp)
        else 
            mapUserNametoTweets <- mapUserNametoTweets.Add(username, list.Empty)
            let mutable temp = mapUserNametoTweets.Item(username)
            temp <- temp @ [retweet]
            mapUserNametoTweets <- mapUserNametoTweets.Add(username, temp)
        totalReTweets <- totalReTweets + 1
        totalTweets<- totalTweets + 1
        printfn "\nLOG: Total Registered Users: %i\nTotal Online Users: %i\nTotal Offline Users: %i\nTotal Tweets: %i\nTotal ReTweets: %i\n" totalUsers loggedInUsers loggedOutUsers totalTweets totalReTweets 
        printfn "%A " mapUserNametoTweets
        Ok{resp = "Done"}     



/// The server side website, tying everything together.
module Site =
    open WebSharper.UI
    open WebSharper.UI.Html
    open WebSharper.UI.Server

    /// Helper function to convert our internal ApiResult type into WebSharper Content.
    let JsonContent (result: ApiResult<'T>) : Async<Content<EndPoint>> =
        match result with
        | Ok value ->
            Content.Json value
        | Error (status, error) ->
            Content.Json error
            |> Content.SetStatus status
        |> Content.WithContentType "application/json"

    /// Respond to an ApiEndPoint by calling the corresponding backend function
    /// and converting the result into Content.
    let ApiContent (ep: ApiEndPoint) : Async<Content<EndPoint>> =
        match ep with
        | CreateUser userData -> JsonContent(Backend.CreateUser userData)
        | LoginUser loginData -> JsonContent(Backend.LoginUser loginData)
        | LogoutUser loginData -> JsonContent(Backend.LogoutUser loginData)
        | PostTweet tweetData -> JsonContent(Backend.PostTweet tweetData)
        | PostFollow followData-> JsonContent(Backend.PostFollow followData)
        | PostSubscribe subscribeData-> JsonContent(Backend.PostSubscribe subscribeData)
        | GetHashtag word -> JsonContent(Backend.GetHashtag word)
        | GetMentiontag word -> JsonContent(Backend.GetMentiontag word)
        | GetRetweet id -> JsonContent(Backend.GetRetweet id)
        | GetTweets -> JsonContent (Backend.GetTweets ())
        | PostRetweets postRetweets-> JsonContent(Backend.PostRetweets postRetweets)
        

    let HomePage (ctx: Context<EndPoint>) : Async<Content<EndPoint>> =
        // Type-safely creates the URI: "/api/people/1"
        let person1Link = ctx.Link (Api (Cors.Of (GetRetweet "1")))
        Content.Page(
            Body = [
                p [] [text "API is running."]
                p [] [
                    text "Try querying: "
                    a [attr.href person1Link] [text person1Link]
                ]
            ]
        )
     
    /// The Sitelet parses requests into EndPoint values
    /// and dispatches them to the content function.
    let Main corsAllowedOrigins : Sitelet<EndPoint> =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | Home -> HomePage ctx
            | Api api ->
                Content.Cors api (fun allows ->
                    { allows with
                        Origins = corsAllowedOrigins
                        Headers = ["Content-Type"]
                    }
                ) ApiContent


        )
