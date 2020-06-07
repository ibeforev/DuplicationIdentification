
# Scoring weights test #
if (sum(unlist(mget(ls(pattern = "Match")))) == 1){
  cat(
    paste0(
      "Scoring weights sum to 1.", "\n",
      "\n"
    )
  )
} else {
  cat(
    paste0(
      "!##| WARNING |##! Scoring weights do not sum to 1.", "\n",
      "\n"
    )
  )
}
  
#################
##| Data Prep |##
#################

# Pull full customer list #
cat(
  paste(
    "Pulling customer list ... "
  )
)
query <- paste0(
"SELECT DISTINCT
	cust.CustomerID
  ,cust.FirstName
  ,cust.LastName
  ,LEN(cust.FirstName) AS LenFirst
  ,cust.EmailID
  ,CAST(cust.DOB AS DATE) AS DOB
  ,DATEDIFF(YEAR, CAST(cust.DOB AS DATE), CAST(GETDATE() AS DATE)) Age
  ,CASE
    WHEN cust.Gender = 1 THEN 'Male'
    WHEN cust.Gender = 2 THEN 'Female'
    ELSE 'Unknown'
   END AS Gender
  ,CAST(cust.LastLogin AS DATE) AS LastLogin
  ,cust.ContactNumber
  ,addy.AddressLine1
  ,addy.AddressLine2
  ,addy.PostalCode
  ,country.CountryName
FROM [dbo].[Customers] cust
  
LEFT JOIN [dbo].[CustomersAddress] addy
  ON cust.CustomerID = addy.CustomerID
LEFT JOIN [dbo].[Countries] country
  ON addy.CountryID = country.CountryID
  
WHERE addy.AddressType = 1 AND DATEDIFF(YEAR, CAST(cust.DOB AS DATE), CAST(GETDATE() AS DATE)) >= 18 AND cust.Status = 1"
)
system.time(allAccounts <- dbGetQuery(zzCon, query))
allAccounts$CustomerID <- as.integer(allAccounts$CustomerID)
allAccounts <- allAccounts[!duplicated(allAccounts$CustomerID), ]
allAccounts$FirstName <- tolower(allAccounts$FirstName)
allAccounts$LastName <- tolower(allAccounts$LastName)
allAccounts$FullName <- paste(allAccounts$FirstName, allAccounts$LastName, sep = " ")
allAccounts$MatchID <- NA
rm(query)
cat(
  paste(
    nrow(allAccounts), " accounts read.", "\n",
    "\n"
  )
)

# Sampling if test run #
if (isTest == "Yes"){
  allAccounts <- as.data.frame(allAccounts[allAccounts$CustomerID %in% sample(allAccounts$CustomerID, sampSize, replace = FALSE), ])
  cat(
    paste0(
      "Test run - ", sampSize, " observations sampled.", "\n",
      "\n"
    )
  )
} else {
  cat(
    paste(
      "Full run - all observations used.", "\n",
      "\n"
    )
  )
}

# Sort by length and alphabetical order #
allAccounts <- as.data.frame(
  allAccounts %>%
    arrange(LenFirst, FirstName, LastName)
)
gc()

###############
##| Scoring |##
###############

# Start global timer #
fullStart <- Sys.time()

# Initiate progress bar #
pb <- winProgressBar(title = "Progress", 
                     label = "0% Done", 
                     min = 0, 
                     max = 100, 
                     initial = 0)

# Loop through, get possible matches by first & last name, increase score based on other matches, calcuate score, assign IDs to possible matches #
i <- 1
while (i <= nrow(allAccounts)){
  if (is.na(allAccounts$MatchID[i])){
    # Get possible matches based on first or last name (Levenshtein distance) - assign score #
    searchTemp <- allAccounts[agrepl(allAccounts$FirstName[i], allAccounts$FirstName, 
                                     max.distance = firstDistance) & 
                                 agrepl(allAccounts$LastName[i], allAccounts$LastName, 
                                        max.distance = lastDistance), ]
    # Test if anything was returned #
    if (nrow(searchTemp) > 1){
      # Assign name match score #
      searchTemp$nameScore <- nameMatch
      # Assign exact full name match score #
      searchTemp$exactNameScore[searchTemp$FullName == allAccounts$FullName[i]] <- fullExactMatch
      # Assign DOB match score #
      searchTemp$dobScore[searchTemp$DOB == allAccounts$DOB[i]] <- dobMatch
      # Assign gender match score #
      searchTemp$genderScore[searchTemp$Gender == allAccounts$Gender[i]] <- genderMatch
      # Assign country match score #
      searchTemp$countryScore[searchTemp$CountryName == allAccounts$CountryName[i]] <- countryMatch
      # Assign address exact match score #
      searchTemp$addressExactScore[searchTemp$AddressLine1 == allAccounts$AddressLine1[i]] <- addressExactMatch
      # Assign address match score #
      if (!is.na(allAccounts$AddressLine1[i]) & nchar(allAccounts$AddressLine1[i]) > 0) {
        searchTemp$addressScore[agrepl(allAccounts$AddressLine1[i], searchTemp$AddressLine1, 
                                       max.distance = addressDistance)] <- addressMatch
      } else {
        searchTemp$addressScore <- NA
      }
      # Total match score #
      searchTemp$matchProb <- rowSums(searchTemp[c("nameScore", "exactNameScore","dobScore", "genderScore", "countryScore", "addressExactScore","addressScore")], na.rm = TRUE)
      # Assign match ID to temp set if score is higher than threshold #
      searchTemp <- searchTemp[searchTemp$matchProb >= matchKeep, ]
      searchTemp$MatchID <- ifelse(
        is.na(searchTemp$MatchID),
        allAccounts$CustomerID[i],
        searchTemp$MatchID
      )
      
      # Assign match ID globally #
      allAccounts$MatchID[allAccounts$CustomerID %in% searchTemp$CustomerID] <- allAccounts$CustomerID[i]
    } else {
      # Assign CustomerID to MatchID if no close name matches are returned #
      allAccounts$MatchID[allAccounts$CustomerID %in% searchTemp$CustomerID] <- allAccounts$CustomerID[i]
    }
    rm(searchTemp, searchSet)
    gc()
  }
  # Update progress bar #
  info <- sprintf("%s%% Done", round((i/nrow(allAccounts)) * 100, 1))
  setWinProgressBar(pb, (i/nrow(allAccounts)) * 100, label = info)
  # Increment #
  i <- i + 1
}
# Close progress bar #
close(pb)
rm(i, pb)
gc()

# Processing time #
fullEnd <- Sys.time()
fullTotal <- round(as.numeric(fullEnd) - as.numeric(fullStart), 0)

# Subset matched duplicate accounts #
duped <- allAccounts[allAccounts$MatchID %in% allAccounts$MatchID[duplicated(allAccounts$MatchID)], ]
duped <- as.data.frame(
  duped %>%
    arrange(MatchID)
)
# Output # 
output <- paste0(
  "Total Processing Time", "\n",
  "---------------------", "\n",
  "Started: ", fullStart, "\n",
  "Finished: ", fullEnd, "\n",
  "  Total time: ",seconds_to_period(fullTotal), "\n",
  "\n",
  "High Level Results", "\n",
  "------------------", "\n",
  "All Accounts (", nrow(allAccounts), ")", "\n",
  "  Duplicate accounts: ", sum(duplicated(allAccounts$MatchID[!is.na(allAccounts$MatchID)])), " (",
      round((sum(duplicated(allAccounts$MatchID[!is.na(allAccounts$MatchID)])) / nrow(allAccounts)) * 100, 1), "%)", "\n",
  "  Unique accounts: ", sum(!duplicated(allAccounts$MatchID[!is.na(allAccounts$MatchID)])), " (",
      round((sum(!duplicated(allAccounts$MatchID[!is.na(allAccounts$MatchID)])) / nrow(allAccounts)) * 100, 1), "%)", "\n",
  "\n",
  "Parameters and Scoring Weights Used", "\n",
  "-----------------------------------", "\n",
  "Parameters", "\n",
  "  Test: ", isTest, "\n",
  "  Sample Size: ", ifelse(isTest == "Yes", sampSize, "Full Run"), "\n",
  "  Matching Threshold: ", round(matchKeep * 100, 0), "%", "\n",
  "  First Name Levenshtein Distance: ", firstDistance, "\n",
  "  Last Name Levenshtein Distance: ", lastDistance, "\n",
  "  Address Levenshtein Distance: ", addressDistance, "\n",
  "\n",
  "Scoring Weights", "\n",
  "  Fuzzy Name Match: ", round(nameMatch, 2), "\n",
  "  Exact Full Name Match: ", round(fullExactMatch, 2), "\n",
  "  DOB Match: ", round(dobMatch, 2) , "\n",
  "  Gender Match: ", round(genderMatch, 2), "\n",
  "  Fuzzy Address Match: ", round(addressMatch, 2), "\n",
  "  Exact Address Match: ", round(addressExactMatch, 2), "\n",
  "  Country Match: ", round(countryMatch, 2), "\n",
  "\n"
)
cat(output)

# Export results #
toWrite <- c("output", "allAccounts", "duped")
for (j in toWrite){
  write.csv(get(j),
            paste0("Output/", as.Date(fullStart), " ",hour(fullStart), ".", minute(fullStart), " ", j, ".csv"),
            quote = TRUE,
            row.names = FALSE,
            na = "")
  cat(
    paste0(
      j, " written to file.", "\n"
    )
  )
}
