namespace LeagueCalculator

open System

type GameOutcome =
| Win
| Loss
| Draw

type GameLocation =
| Home
| Away

type TeamResult = {
        Team: string
        Opposition: string
        Location: GameLocation
        PlayedOn: DateTime
        GoalsFor: int
        GoalsAgainst: int
    } with
    member this.Outcome =
        if this.GoalsFor > this.GoalsAgainst then Win
        elif this.GoalsFor = this.GoalsAgainst then Draw 
        else Loss

type GameResult = {
        PlayedOn: DateTime
        HomeTeam: string
        AwayTeam: string
        HomeGoals: int
        AwayGoals: int
    } with
    member this.HomeTeamResult =
        { Team = this.HomeTeam; Opposition = this.AwayTeam; Location = Home; PlayedOn = this.PlayedOn; GoalsFor = this.HomeGoals; GoalsAgainst = this.AwayGoals }
    member this.AwayTeamResult = 
        { Team = this.AwayTeam; Opposition = this.HomeTeam; Location = Away; PlayedOn = this.PlayedOn; GoalsFor = this.AwayGoals; GoalsAgainst = this.HomeGoals }

type Standing = {
    Team: string
    Points: int
    GoalsFor: int
    GoalsAgainst: int
    GoalDifference: int
    GamesPlayed: int
}

module Games =
   
    let initialStanding team = { Team = team; Points = 0; GoalsFor = 0; GoalsAgainst = 0; GoalDifference = 0; GamesPlayed = 0 }

    let toStanding calcPoints (lastStanding: Standing) (teamResult: TeamResult) : Standing =
        {   Team = teamResult.Team; 
            Points = lastStanding.Points + (calcPoints teamResult.Outcome); 
            GoalsFor = lastStanding.GoalsFor + teamResult.GoalsFor; 
            GoalsAgainst = lastStanding.GoalsAgainst + teamResult.GoalsAgainst; 
            GoalDifference = lastStanding.GoalDifference + teamResult.GoalsFor - teamResult.GoalsAgainst; 
            GamesPlayed = lastStanding.GamesPlayed + 1 }
    
    let filterByTeam team = List.filter (fun standing -> standing.Team = team)    
    
    let latestStandingForTeam team standings = 
        let teamStandings = standings |> filterByTeam team
        match teamStandings with
        | [] -> initialStanding team
        | _ -> teamStandings 
            |> List.maxBy (fun standing -> standing.GamesPlayed)     
                                       
    let appendResult calcPoints (teamResult:TeamResult) standings = 
        
        let toStandingWithPoints = toStanding calcPoints
        standings 
        |> latestStandingForTeam teamResult.Team
        |> toStandingWithPoints 
        <| teamResult   
        |> Array.create 1 
        |> List.ofArray
        |> List.append standings

    let rec addGames calcPoints (games:GameResult list) standings =
        match games with
        | [] -> standings
        | game :: remainingGames -> 
            standings
            |> appendResult calcPoints game.HomeTeamResult
            |> appendResult calcPoints game.AwayTeamResult 
            |> addGames calcPoints remainingGames

    let toResults calcPoints games =
        [] |> addGames calcPoints games

    let addStanding previousStandings standing =
        { standing with 
            Points = previousStandings.Points + standing.Points; 
            GoalsFor = previousStandings.GoalsFor + standing.GoalsFor; 
            GoalsAgainst = previousStandings.GoalsAgainst + standing.GoalsAgainst; 
            GoalDifference = previousStandings.GoalDifference + standing.GoalDifference;
            GamesPlayed = previousStandings.GamesPlayed + 1
        }  

    let createLeagueTable sort standings = 
        standings
        // Group by team
        |> Seq.groupBy (fun standing -> standing.Team)
        // Get the team's last result
        |> Seq.map (fun teamStandings -> 
            let team, standings = teamStandings
            standings |> Seq.maxBy (fun standing -> standing.GamesPlayed)
        )
        // Sort results by points, then goal difference
        |> sort

module FootballData =
    
    open FSharp.Data

    type League = string
    
    let EnglishPremierLeague : League = "E0"
    let SpanishLaLiga : League = "SP1"

    let getData (seasonStartYear:int) (league: League) = 
        
        let cultureInfo = new Globalization.CultureInfo("en-GB")

        let convertRow (row: CsvRow) =
            {   
                PlayedOn = (row.["Date"].AsDateTime(cultureInfo))
                HomeTeam = (row.["HomeTeam"]); 
                HomeGoals = (row.["FTHG"].AsInteger()); 
                AwayTeam = (row.["AwayTeam"]); 
                AwayGoals = (row.["FTAG"].AsInteger())}
        
        let url = sprintf "http://www.football-data.co.uk/mmz4281/%2i%2i/%s.csv" seasonStartYear (seasonStartYear + 1) league 
        
        CsvFile.Load(url).Cache().Rows
        |> Seq.map convertRow

module PremierLeague =    

    let getData () = FootballData.getData 15 FootballData.EnglishPremierLeague

    let calcPoints result = 
        match result with
        | Win -> 3
        | Draw -> 1
        | Loss -> 0
                                                                                                        
    let order standings = 
        standings
        |> Seq.sortByDescending (fun standing -> standing.Points, standing.GoalDifference, standing.GoalsFor, standing.Team)    

module LaLiga =

    let getData () = FootballData.getData 15 FootballData.SpanishLaLiga

    let calcPoints result = 
        match result with
        | Win -> 3
        | Draw -> 1
        | Loss -> 0

    let order standings = 
        standings
        |> Seq.sortByDescending (fun standing -> standing.Points, standing.GoalDifference, standing.GoalsFor, standing.Team)

module Program = 
            
    let displayTableInConsole table =
       
        let cprintfn c =
            Printf.kprintf (fun s ->
                    let orig = Console.ForegroundColor
                    Console.ForegroundColor <- c
                    Console.WriteLine(s)
                    Console.ForegroundColor <- orig)
                        
        // Print table heading
        cprintfn ConsoleColor.Red "%-3s %-30s %-2s %-3s %-3s %-3s %-2s" "Pos" "Team" "Pd" "GD" "GF" "GA" "Pt"

        let printRow pos result = printfn "%-3i %-30s %2i %3i %3i %3i %2i" (pos+1) result.Team result.GamesPlayed result.GoalDifference result.GoalsFor result.GoalsAgainst result.Points

        // Print results
        table |> Seq.iteri printRow 

    [<EntryPoint>]
    let main args =
       
        // Load data for Premier league games
        PremierLeague.getData()
        |> Seq.filter (fun r -> r.PlayedOn < DateTime.Now.AddDays(-14.0) )
        |> Seq.toList
        // Add all the games from the data source
        |> Games.toResults PremierLeague.calcPoints
        // Create a table
        |> Games.createLeagueTable PremierLeague.order
        // Display the table
        |> displayTableInConsole

        // Add blank line
        Console.WriteLine()
        
        Console.ReadKey() |> ignore

        // Load data for games
        LaLiga.getData() 
        |> Seq.toList
        // Add all the games from the data source
        |> Games.toResults LaLiga.calcPoints
        // Create a table
        |> Games.createLeagueTable LaLiga.order
        // Display the table
        |> displayTableInConsole
                                                     
        Console.ReadKey() |> ignore

        0
