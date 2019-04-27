# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




#' Roll your dice
#'
#' @param numrolls The amount of times you wish to roll
#' @param numdice How many sides of the dice you wish to roll (6 sided die, 8 sided die...)
#' @param nameroller The name of the character that is rolling (use "quotation marks" for character name)
#' @param display How you wish to see the results of the roll ("combine" for the total, "split" for the results of each roll)
#'
#' @return The name of the character who rolled amd the results in the display type asked for as a list
#' @export
#'
#' @examples playeroneroll <- roll (4,6,"Fred","combine")
roll <- function(numrolls,numdice,nameroller,display){
  resultsum = 0


  if(display == "combine"){
    result <- round(runif(numrolls,1,numdice))
    resultsum <- sum(result,na.rm = FALSE)
    nameroller <- c(nameroller, resultsum)

  } else if (display == "split") {
    result <- round(runif(numrolls,1,numdice))
    nameroller <- c(nameroller, result)
  } else{

    print("Invalid entry. Please enter 'split' or 'combine'.")
  }

  return(nameroller)

}


#' Import Character
#'
#' @param charinfo Charinfo is a list type that saves information on a character.
#'             This character information can also be updated with this funciton as well
#'             The information stored is the characters: Name, Strength, Dexterity, constituion, intelligence,
#'             wisdom, and charisma
#' @param action the action parameter tells the function whether you wish to create or update a character list.
#'         If you wish to create a character use (with quotations) "create"  and if you wish to update a character
#'         use "update".
#'
#' @return character returns a list with character information inside
#' @export
#'
#' @examples playerone <- character(Fred, "create")
character <- function(charinfo,action){

  if(action == "create"){

    charname <- readline("Enter character's name: ")
    charstr <- readline("Enter character's strength: ")
    chardex <- readline("Enter character's dexterity: ")
    charcon <- readline("Enter character's constitution: ")
    charint <- readline("Enter character's intelligence: ")
    charwis <- readline("Enter character's wisdom: ")
    charchar <- readline("Enter character's charisma: ")

    charinfo = list(charname, charstr, chardex, charcon, charint, charwis, charchar)
    print(charinfo)

  } else if(action == "update"){

    y <- readline("Enter stat you wish to change: ")

    if (y == "strength"){

        charinfo[2] <- readline("New strength: ")
    }else if(y == "dexterity"){

      charinfo[3] <- readline("New dexterity: ")
    }else if(y == "constitution"){

      charinfo[4] <- readline("New constitution: ")
    }else if(y == "intelligence"){

      charinfo[5] <- readline("New intelligence: ")
    }else if(y == "wisdom"){

      charinfo[6] <- readline("New wisdom: ")
    }else if(y == "charisma"){

      charinfo[7] <- readline("New charisma")
    }

  }else if(action == "display"){

    print(charinfo)
  }



  return(charinfo)
}
