#' Comparación de Medias
#'
#' Realiza una tabla ANOVA para la comparación de medias de tres tratamientos.
#'
#' @param df base de datos
#' @param A (Vector) tratamiento 1.
#' @param B (Vector) tratamiento 2.
#' @param C (Vcetor) tratamiento 3.
#' @export
#'
#' @examples
#' /dontrun{
#' ##Directorio de trabajo
#' ruta <- "~/ruta/test.csv"
#'
#' ------------------------------
#' #Ejemplo 1
#' df <- read.csv(ruta)
#' # Cargo la libreria
#' library(ComMedia)
#' A, B Y C representan los tres tratamientos
#' Com_Media(A = df$A, B = df$B, C = df$C)
#'
#' ------------------------------
#' # Ejemplo 2
#' A <- c(39.5,41.2,38.8,42.1,40.4,37.9,44.2,42.7,36.5,45.8,40.1,43.6,39.9,42.3,38.2,46.5,41.7,38.9,47.9,40.8)
#' B <- c(42.1,44.3,41.8,45.6,43.9,40.5,47.2,46.1,39.7,48.8,43.2,46.9,42.6,45.3,41.4,48.2,44.6,41.9,49.7,43.8)
#' c <- c(45.2,47.5,46.8,44.3,46.6,43.9,51.1,48.7,42.4,50.845.9,48.2,46.6,49.1,44.8,52.6,49.3,47.1,53.4,46.7)
#'
#' # Cargo la función
#' Com_Media(A = A, B = B, C = C)
#' }
Come <- function(A, B, C) {

  # Número de veces tratado cada uno
  NK <- c(length(A), length(B), length(C))
  n <- sum(NK)
  K <- (3)

  # Suma de cada tratamiento
  (suma_A <- sum(A))
  (suma_B <- sum(B))
  (suma_C <- sum(C))

  # Suma total de los tratamientos
  S_total <- suma_A + suma_B + suma_C
  print(S_total)

  # Factor de correlación
  FC <- (S_total^2)/(n)
  print(FC)

  # Suma de cuadrados total (SCT)
  SCT <- sum((A)^2, (B)^2, (C)^2) - FC
  print(SCT)

  # suma de cuadrados de los tratamientos
  A_2 <- sum(A)^2
  B_2 <- sum(B)^2
  C_2 <- sum(C)^2

  # Suma de cuadrados
  X <- sum(A_2, B_2, C_2)

  # Suma de cuadrados Tratamientos (SCTr)
  n_1 <- c(20)
  SCTr <- (X / n_1) - FC
  print(SCTr)

  # Suma de cuadrados del Error (SCE)
  SCE <- (SCT - SCTr)
  print(SCE)

  # Grados de libertad (GL)
  GL <- c(K-1, n-K, n-1)
  print(GL)

  # Cudrados medios (CM)
  CMTr <- (SCTr / (K - 1))
  print(CMTr)
  CME <- (SCE / (n - K))

  # Razón F
  R_f <- CMTr / CME
  print(R_f)

  # Crear un dataframe con la Tabla ANOVA de tratamientos
  tab_Anova <- data.frame(F.Var = c("Tratamiento", "Error", "Total"),
                          Gl = c(K-1, n-K, n-1),
                          SC = c(SCTr, SCE, SCT),
                          CM = c(CMTr, CME, NA),
                          Razón_f = c(R_f, NA, NA))

  return(tab_Anova)
}
