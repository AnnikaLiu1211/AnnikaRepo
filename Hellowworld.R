
# object assignment
hello_world <- function(name = NULL) {
  # conditional statement: greeting based on if there is a name 
  message <- if (!is.null(name) && nchar(name) > 0) {
      # learning notes: !is.null(name) will be true if the name is not Null
      # nchar helps to to check whether the number of charecters in the string is 0
    paste0("Hello ", name, "!")
  } else {
    "Hello!"
  }
  print(message)
}

 # return value 1
hello_world("Annika")
 # return value 2
hello_world()


