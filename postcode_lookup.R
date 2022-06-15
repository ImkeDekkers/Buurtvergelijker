# Function that takes a given zip code as input, and has the municipality, neighbourhood and borough name as output
postcode_lookup <- function(postcodes_final, pc_input) {
  #Replace white space with empty spring and change to upper case letters
  preprocessed_postcode <- toupper(str_replace_all(pc_input, " ", ""))
  
  pc_match <- postcodes_final |> filter(PC6 == preprocessed_postcode)
  if (nrow(pc_match) == 0) return(print("Er is (nog) geen geldige postcode ingevoerd."))
  if (nrow(pc_match) == 1) return(
    with(pc_match, sprintf('Uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s', Gemeentenaam2020, wijknaam2020, buurtnaam2020))
  )
  else return(
    sprintf("Uw postcode komt voor in meerdere gebieden. Uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s of uw gemeentenaam is %s, uw wijknaam is %s en uw buurtnaam is %s", 
            pc_match$Gemeentenaam2020[1], pc_match$wijknaam2020[1], pc_match$buurtnaam2020[1], pc_match$Gemeentenaam2020[2], pc_match$wijknaam2020[2], pc_match$buurtnaam2020[2])
  )
}