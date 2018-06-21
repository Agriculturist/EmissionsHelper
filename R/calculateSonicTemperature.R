#' calculateSonicTemperature 
#'
#' This function calculates the sonic virtual temperature in air based on the measured speed of sound.
#' 
#' Results are in degrees Celcius.
#'
#' @param speedOfSound the speed of sound in m/s
#' @param GammaConstant ratio of specific heat of dry air at constant pressure to that at a constant volume
#' @param Rd ideal gas constant for dry air (units J K^-1 kg^-1)
#' @keywords emissions
#' @examples
#'
#' @export
#' 
#' 
#' 

calculateSonicTemperature <- function(speedOfSound, GammaConstant = 1.4, Rd = 287.04){
  return(speedOfSound^2 / (GammaConstant * R_d)  - 273.15)
}
