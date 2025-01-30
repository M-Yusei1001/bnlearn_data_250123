library(gRain)

junction <- compile(as.grain(fitted))

test = list(金属_S=1,鋭利_S=1)

jstates <- junction |> evidence_add(test)

jstates |> evidence_get()
jstates |> evidence_prob()