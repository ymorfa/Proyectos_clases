data <- read.csv("~/Documentos/CIMAT/Semester_4/Patten_Recognition/homeworks/Tarea_9/data/210531COVID19_MEXICO.csv")

data = data[data$ENTIDAD_UM==15,]

copy  = data

cols = c('SEXO', 'TIPO_PACIENTE', 'NEUMONIA', 'EDAD',
         'DIABETES', 'EPOC', 'ASMA', 'INMUSUPR', 'HIPERTENSION', 
         'CARDIOVASCULAR', 'OBESIDAD', 'RENAL_CRONICA', 'TABAQUISMO')

data = data[, cols]

### SEXO

data$TIPO_PACIENTE = data$TIPO_PACIENTE - 1
logit <- glm(TIPO_PACIENTE~., data, family=binomial(link="logit"))
summary(logit)

pred = round(predict(logit, data, type = 'response'))

acc = mean(pred == data$TIPO_PACIENT)

cols = c('SEXO', 'TIPO_PACIENTE', 'NEUMONIA', 'EDAD',
         'CARDIOVASCULAR', 'OBESIDAD', 'RENAL_CRONICA', 'TABAQUISMO')

data1 = data[, cols]
logit <- glm(TIPO_PACIENTE~., data1, family=binomial(link="logit"))
summary(logit)

pred = round(predict(logit, data1, type = 'response'))

acc = mean(pred == data$TIPO_PACIENT)
