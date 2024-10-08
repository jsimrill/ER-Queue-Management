library(simmer)

Resource <- setRefClass("Resource", 
                        fields = list(id = "Character", availability = "logical"),
                        methods = list(
                          allocate = function() {
                            availability <<- FALSE
                            cat("Resource", id, "allocated.\n")
                          }
                        )
                      )

Doctor <-setRefClass("Doctor", contains = "Resource")

Nurse <- setRefClass("Nurse", contains = "Resource")

Bed <- setRefClass("Bed", contains = "Resource", 
                   fields = list(roomNumber = "character"))

Patient <- setRefClass("Patient",
                       fields = list(arrivalTime = "numeric",
                                     treatmentTime = "numeric",
                                     dischargeTime = "numeric"),
                       methods = list(
                         getTreated = function() {
                           cat("Patient treated for", treatmentTime, "minutes.\n")
                           dischargeTime <<- arrivalTime + treatmentTime
                         }
                       )
                     )

#Queue Management
