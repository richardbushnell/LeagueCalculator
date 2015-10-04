namespace LeagueCalculator

type GameScore = {
    HomeTeam: string
    AwayTeam: string
    HomeGoals: int
    AwayGoals: int
}

type GameResult = {
    Team: string
    Points: int
    GoalsFor: int
    GoalsAgainst: int
    GoalDifference: int
    GamesPlayed: int
}

module Games =
    
    let calcPoints goalsFor goalsAgainst = 
        if goalsFor > goalsAgainst then 3
        elif goalsFor = goalsAgainst then 1 
        else 0

    let resultsForGame game =
        let homeTeamResult = { Team = game.HomeTeam; Points = (calcPoints game.HomeGoals game.AwayGoals); GoalsFor = game.HomeGoals; GoalsAgainst = game.AwayGoals; GoalDifference = game.HomeGoals - game.AwayGoals; GamesPlayed = 1 }
        let awayTeamResult = { Team = game.AwayTeam; Points = (calcPoints game.AwayGoals game.HomeGoals); GoalsFor = game.AwayGoals; GoalsAgainst = game.HomeGoals; GoalDifference = game.AwayGoals - game.HomeGoals; GamesPlayed = 1 }
        homeTeamResult, awayTeamResult

    let addGame game previousResults = 
        let homeTeamResult, awayTeamResult = resultsForGame game
        previousResults @ [homeTeamResult; awayTeamResult]

    let rec addGames games results =
        match games with
        | [] -> results
        | game :: remainingGames -> 
            addGame game results 
            |> addGames remainingGames

    let addResult previousResults result =
        { result with 
            Points = previousResults.Points + result.Points; 
            GoalsFor = previousResults.GoalsFor + result.GoalsFor; 
            GoalsAgainst = previousResults.GoalsAgainst + result.GoalsAgainst; 
            GoalDifference = previousResults.GoalDifference + result.GoalDifference;
            GamesPlayed = previousResults.GamesPlayed + 1
        }  

    let initialResults team = { Team = team; Points = 0; GoalsFor = 0; GoalsAgainst = 0; GoalDifference = 0; GamesPlayed = 0 }

    let createLeagueTable results = 
        results
        // Group by team
        |> Seq.groupBy (fun result -> result.Team)
        // Add up each team's results
        |> Seq.map (fun teamResults -> 
            let team, results = teamResults
            // Add up the results for the team
            results
            |> Seq.fold addResult (initialResults team)
        )
        // Sort results by points, then goal difference
        |> Seq.sortByDescending (fun standing -> standing.Points, standing.GoalDifference, standing.GoalsFor, standing.Team)

    let displayTableInConsole results =
        
        // Print table heading
        printfn "%-30s %-2s %-3s %-3s %-3s %-2s" "Team" "Pd" "GD" "GF" "GA" "Pt"

        let printRow result = printfn "%-30s %2i %3i %3i %3i %2i" result.Team result.GamesPlayed result.GoalDifference result.GoalsFor result.GoalsAgainst result.Points

        // Create a table
        createLeagueTable results
        // Print results
        |> Seq.iter printRow 
            
    open FSharp.Data

    let getWebData () = 
        
        let gameData = CsvFile.Load("http://www.football-data.co.uk/mmz4281/1516/E0.csv").Cache()

        let convertRow (row: CsvRow) =
            {   HomeTeam = (row.["HomeTeam"]); 
                HomeGoals = (row.["FTHG"].AsInteger()); 
                AwayTeam = (row.["AwayTeam"]); 
                AwayGoals = (row.["FTAG"].AsInteger())}
        
        gameData.Rows
        |> Seq.map convertRow

    [<EntryPoint>]
    let main args =
        // Load data for games
        let games = getWebData () |> Seq.toList
        // Start with empty list of results
        []
        // Add all the games from the data source
        |> addGames games
        // Display the table
        |> displayTableInConsole

        System.Console.ReadKey() |> ignore
        
        0
