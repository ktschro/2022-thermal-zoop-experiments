#First crack at a nice daphnia model

library(deSolve)
#Marta's temperature dependent model - 2018 AmNat Paper - Temperature Drives Epidemics in a Zooplankton-Fungus Disease System
daphnia_eqn<- function(time,variables,parameters) {
  with(as.list(c(variables,parameters)), {
    dS <- e*f*A*(S+I) - d*S - beta*S*Z
    dI <- beta*S*Z - di*I
    dZ <- di*I*sigma*(A/(A+h)) - m*Z - f*(S+I)*Z
    dA <- r*A*(1-A/K)-f*(S+i)*A
    return(list(c(dS,dI,dZ,dA)))
  })
}

#defining values for parameters
daphnia_parameters<- c(
  e = #conversion efficiency
    f= #foraging rate
    d= #background death rate
    beta= #transmission rate
    di= #infected host death rate
    sigma= #maximum spore yield
    h= #half-saturation constant
    m= #rate of loss of spores
    r= #maximum per capita growth rate of algae
    K=#carrying capacity of algae
)

#set initial values for S, I, and R
inital_values<- c(
  S= 1000 #number of susceptibles at t=0
    I= 0 #number of infectious at t=0
    Z= 1000000 #density of spores at t=0
    R= 60 #algae at t=0
)

#set up number of days
time_values <- seq(0,60) #number of days

#use ode to solve
daphnia_eqn_solve <- ode(
  y= initial_values,
  times= time_values,
  func= daphnia_eqn,
  parms= daphnia_parameters
)








