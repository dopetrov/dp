library(XML)
sumcost = function(data){
  n = nrow(data[6])
  if (n > 0){
    sum(as.numeric(gsub(",", ".", data[1:n,6])))
  }
  else{
    return(0)
  }
}
sumdur = function(data){
  n = nrow(data[4])
  if (n > 0){
    sum(as.numeric(gsub(":", ".", data[1:n, 4])))
  }
  else{
    return(0)
  }
}
summins = function(data){
  n = nrow(data[5])
  if (n > 0){
    sum(as.numeric(gsub(":", ".", data[1:n, 5])))
  }
  else{
    return(0)
  }
}
mtsresum = function(data){
  ans1 = 0
  ans2 = 0
  for (i in 1:length(data))
  {
    ans1[i] = sum(as.numeric(gsub(":", ".", data[[i]][[5]])))
  }
  ans1 = ans1[ans1 > 20]
  if (length(ans1) == 0)
  {
    return(0)
  }
  for (i in 1:length(ans1))
  {
    ans2[i] = (ans1[i] - 20) * 0.75
  }
  ans2 = sum(ans2)
  return(ans2)
}
tele2resum = function(data){
  ans1 = 0
  ans2 = 0
  ans3 = 0
  ans4 = 0
  if (length(data[[5]]) == 0)
  {
    return(0)
  }
  for (i in 1:length(data[[5]]))
  {
    ans1[i] = as.numeric(gsub(":", ".", data[i,5]))
  }
  ans2 = ans1[ans1 > 1]
  if (length(ans2) == 0)
  {
    ans2 = 0
  }
  ans3 = ans1[ans1 == 1]
  if (length(ans3) == 0)
  {
    ans3 = 0
  }
  for (i in 1:length(ans2))
  {
    ans4[i] = (ans2[i] - 1) * 0.2 + 0.6
  }
  if (length(ans4) == 0)
  {
    ans4 = 0
  }
  ans4 = sum(ans4) + (sum(ans3) * 0.6)
  return(ans4)
}

shinyServer(
  function(input, output){
    output$ui = renderUI({
      if (is.null(input$file)) { return(NULL) }
      selectInput('selection', 'Выберите тарифный план:', 
                  c('Исходный' = 'in1', 'МТС - "Супер МТС"' = 'in2',
                    'МТС - "Посекундный"' = 'in3', 'Теле2 - "Оранжевый"' = 'in4', 'Теле2 - "Желтый"' = 'in5', 
                    'Теле2 - "Бирюзовый"' = 'in6', 'Билайн - "Go!"' = 'in7', 'Билайн - "Добро пожаловать"' = 'in8',
                    'МегаФон - "Переходи на НОЛЬ"' = 'in9', 'МегаФон - "Всё просто"' = 'in10'), selected = 'in1')
    })
    output$result1 = renderTable({
      if (is.null(input$file)) { return(NULL) }
      attrs = xpathApply(xmlParse(input$file$datapath), "//*/i", xmlAttrs)
      len = length(attrs)
      a = c(1:len)
      b = c(1:len)
      c = c(1:len)
      d = c(1:len)
      e = c(1:len)
      f = c(1:len)
      g = c(1:len)
      maindata = data.frame(a, b, c, d, e, f, g)
      names(maindata) = c("Date/Time", "Number", "Service", "Duration", "Minutes", "Cost", "Cost (Tax)")
      for (i in 1:len)
      {
        if (length(attrs[[i]]) == 13)
        {
          maindata[i,1] = attrs[[i]][[1]]
          maindata[i,2] = attrs[[i]][[2]]
          maindata[i,3] = attrs[[i]][[5]]
          maindata[i,3] = iconv(maindata[i,3], from = "UTF-8", to = "UTF-8")
          maindata[i,4] = attrs[[i]][[7]]
          maindata[i,5] = attrs[[i]][[9]]
          maindata[i,6] = attrs[[i]][[8]]
          maindata[i,7] = attrs[[i]][[10]]
        }
        else
        {
          maindata[i,1] = attrs[[i]][[1]]
          maindata[i,2] = attrs[[i]][[2]]
          maindata[i,3] = attrs[[i]][[5]]
          maindata[i,3] = iconv(maindata[i,3], from = "UTF-8", to = "UTF-8")
          maindata[i,4] = attrs[[i]][[7]]
          maindata[i,5] = attrs[[i]][[10]]
          maindata[i,6] = attrs[[i]][[8]]
          maindata[i,7] = attrs[[i]][[11]]
        }
      }
      maindata = maindata[grep("0", maindata$Cost, invert = TRUE),]
      
      allcallsinfo = maindata[grep("Тел", maindata$Service, invert = FALSE),]
      smsinfo = maindata[grep("sms", maindata$Service, invert = FALSE),]
      mtscallsinfo = allcallsinfo[grep("(7981)|(7911)|(7910)", allcallsinfo$Number, invert = FALSE),]
      beelinecallsinfo = allcallsinfo[grep("(7960)|(7909)|(7963)", allcallsinfo$Number, invert = FALSE),]
      megafoncallsinfo = allcallsinfo[grep("7921", allcallsinfo$Number, invert = FALSE),]
      tele2callsinfo = allcallsinfo[grep("7953", allcallsinfo$Number, invert = FALSE),]
      othercallsinfo = allcallsinfo[grep("(7981)|(7911)|(7910)|(7960)|(7909)|(7963)|(7921)|(7953)", allcallsinfo$Number, invert = TRUE),]
      smstotele2 = smsinfo[grep("7953", smsinfo$Number, invert = FALSE),]
      smstomts = smsinfo[grep("(7981)|(7911)|(7910)", smsinfo$Number, invert = FALSE),]
      smstomegafon = smsinfo[grep("7921", smsinfo$Number, invert = FALSE),]
      smstobeeline = smsinfo[grep("(7960)|(7909)|(7963)", smsinfo$Number, invert = FALSE),]
      smstoother = smsinfo[grep("(7981)|(7911)|(7910)|(7960)|(7909)|(7963)|(7921)|(7953)", smsinfo$Number, invert = TRUE),]
        
      a = c(1,1,1,1,1,1)
      b = c(1,1,1,1,1,1)
      c = c(1,1,1,1,1,1)
      d = c(1,1,1,1,1,1)
      e = c(1,1,1,1,1,1)
      f = c(1,1,1,1,1,1)
      row.names = c("Tele2", "MTS", "MegaFon", "Beeline", "Other", "All")
      df = data.frame(a, b, c, d, e, f, row.names = row.names)
      names(df) = c("Calls", "SMS", "Duration", "Minutes", "Cost (Calls)", "Cost (SMS)")
      
      df[1,1] = nrow(tele2callsinfo[1])
      df[2,1] = nrow(mtscallsinfo[1])
      df[3,1] = nrow(megafoncallsinfo[1])
      df[4,1] = nrow(beelinecallsinfo[1])
      df[5,1] = nrow(othercallsinfo[1])
      df[6,1] = nrow(allcallsinfo[1])
      
      df[1,2] = nrow(smstotele2[1])
      df[2,2] = nrow(smstomts[1])
      df[3,2] = nrow(smstomegafon[1])
      df[4,2] = nrow(smstobeeline[1])
      df[5,2] = nrow(smstoother[1])
      df[6,2] = nrow(smsinfo[1])
      
      df[1,3] = sumdur(tele2callsinfo)
      df[2,3] = sumdur(mtscallsinfo)
      df[3,3] = sumdur(megafoncallsinfo)
      df[4,3] = sumdur(beelinecallsinfo)
      df[5,3] = sumdur(othercallsinfo)
      df[6,3] = sumdur(allcallsinfo)
      
      df[1,4] = summins(tele2callsinfo)
      df[2,4] = summins(mtscallsinfo)
      df[3,4] = summins(megafoncallsinfo)
      df[4,4] = summins(beelinecallsinfo)
      df[5,4] = summins(othercallsinfo)
      df[6,4] = summins(allcallsinfo)
      
      df[1,5] = sumcost(tele2callsinfo)
      df[2,5] = sumcost(mtscallsinfo)
      df[3,5] = sumcost(megafoncallsinfo)
      df[4,5] = sumcost(beelinecallsinfo)
      df[5,5] = sumcost(othercallsinfo)
      df[6,5] = sumcost(allcallsinfo)
      
      df[1,6] = sumcost(smstotele2)
      df[2,6] = sumcost(smstomts)
      df[3,6] = sumcost(smstomegafon)
      df[4,6] = sumcost(smstobeeline)
      df[5,6] = sumcost(smstoother)
      df[6,6] = sumcost(smsinfo)
      
      df[[1]] = as.integer(df[[1]])
      df[[2]] = as.integer(df[[2]])
      df[[4]] = as.integer(df[[4]])
      
      output$result2 = renderTable({
        if (nrow(mtscallsinfo) > 0)
        {
          mtsreinfo = mtscallsinfo
          for (i in 1:nrow(mtscallsinfo))
          {
            mtsreinfo[[1]][[i]] = strsplit(mtscallsinfo[[1]][[i]], " ")[[1]][[1]]
          }
          mtsreinfo = split(mtscallsinfo, mtscallsinfo$Date)
          df[2,5] = mtsresum(mtsreinfo)
        }
        else
        {
          df[2,5] = 0
        }
        
        df[1,5] = df[1,4] * 1.3
        df[3,5] = df[3,4] * 1.3
        df[4,5] = df[4,4] * 1.3
        df[5,5] = df[5,4] * 1.3
        df[6,5] = df[6,4] * 1.3
        
        df[1,6] = df[1,2] * 1.5
        df[2,6] = df[2,2] * 1.5
        df[3,6] = df[3,2] * 1.5
        df[4,6] = df[4,2] * 1.5
        df[5,6] = df[5,2] * 1.5
        df[6,6] = df[6,2] * 1.5
        
        df
      })
      
      output$result3 = renderTable({
        df[1,5] = df[1,3] * 60 * 0.03
        df[2,5] = df[2,3] * 60 * 0.03
        df[3,5] = df[3,3] * 60 * 0.03
        df[4,5] = df[4,3] * 60 * 0.03
        df[5,5] = df[5,3] * 60 * 0.03
        df[6,5] = df[6,3] * 60 * 0.03
        
        df[1,6] = df[1,2] * 1.5
        df[2,6] = df[2,2] * 1.5
        df[3,6] = df[3,2] * 1.5
        df[4,6] = df[4,2] * 1.5
        df[5,6] = df[5,2] * 1.5
        df[6,6] = df[6,2] * 1.5
        
        df
      })
      
      output$result4 = renderTable({
        df[1,5] = df[1,3] * 60 * 0.01
        df[2,5] = df[2,3] * 60 * 0.01
        df[3,5] = df[3,3] * 60 * 0.01
        df[4,5] = df[4,3] * 60 * 0.01
        df[5,5] = df[5,3] * 60 * 0.01
        df[6,5] = df[6,3] * 60 * 0.01
        
        df[1,6] = df[1,2] * 1
        df[2,6] = df[2,2] * 1
        df[3,6] = df[3,2] * 1
        df[4,6] = df[4,2] * 1
        df[5,6] = df[5,2] * 1
        df[6,6] = df[6,2] * 1
        
        df
      })
      
      output$result5 = renderTable({
        df[1,5] = df[1,1] * 0.25
        df[2,5] = df[2,4] * 1
        df[3,5] = df[3,4] * 1
        df[4,5] = df[4,4] * 1
        df[5,5] = df[5,4] * 1
        df[6,5] = df[6,4] * 1
        
        df[1,6] = df[1,2] * 0.5
        df[2,6] = df[2,2] * 0.5
        df[3,6] = df[3,2] * 0.5
        df[4,6] = df[4,2] * 0.5
        df[5,6] = df[5,2] * 0.5
        df[6,6] = df[6,2] * 0.5
        
        df
      })
      
      output$result6 = renderTable({
        df[1,5] = tele2resum(tele2callsinfo)
        df[2,5] = tele2resum(mtscallsinfo)
        df[3,5] = tele2resum(megafoncallsinfo)
        df[4,5] = tele2resum(beelinecallsinfo)
        df[5,5] = tele2resum(othercallsinfo)
        df[6,5] = tele2resum(allcallsinfo)

        
        df[1,6] = df[1,2] * 1
        df[2,6] = df[2,2] * 1
        df[3,6] = df[3,2] * 1
        df[4,6] = df[4,2] * 1
        df[5,6] = df[5,2] * 1
        df[6,6] = df[6,2] * 1
        
        df
      })
      
      output$result7 = renderTable({
        df[1,5] = df[1,4] * 0.6
        df[2,5] = df[2,4] * 0.6
        df[3,5] = df[3,4] * 0.6
        df[4,5] = df[4,4] * 0.6
        df[5,5] = df[5,4] * 0.6
        df[6,5] = df[6,4] * 0.6
        
        df[1,6] = df[1,2] * 0.99
        df[2,6] = df[2,2] * 0.99
        df[3,6] = df[3,2] * 0.99
        df[4,6] = df[4,2] * 0.99
        df[5,6] = df[5,2] * 0.99
        df[6,6] = df[6,2] * 0.99
        
        df
      })
      
      output$result8 = renderTable({
        df[1,5] = df[1,4] * 0.6
        df[2,5] = df[2,4] * 0.6
        df[3,5] = df[3,4] * 0.6
        df[4,5] = df[4,4] * 0.6
        df[5,5] = df[5,4] * 0.6
        df[6,5] = df[6,4] * 0.6
        
        df[1,6] = df[1,2] * 1.5
        df[2,6] = df[2,2] * 1.5
        df[3,6] = df[3,2] * 1.5
        df[4,6] = df[4,2] * 1.5
        df[5,6] = df[5,2] * 1.5
        df[6,6] = df[6,2] * 1.5
        
        df
      })
      
      output$result9 = renderTable({
        df[1,5] = df[1,4] * 1.5
        df[2,5] = df[2,4] * 1.5
        df[3,5] = df[3,1] * 1.5
        df[4,5] = df[4,4] * 1.5
        df[5,5] = df[5,4] * 1.5
        df[6,5] = df[6,4] * 1.5
        
        df[1,6] = df[1,2] * 1.6
        df[2,6] = df[2,2] * 1.6
        df[3,6] = df[3,2] * 1.6
        df[4,6] = df[4,2] * 1.6
        df[5,6] = df[5,2] * 1.6
        df[6,6] = df[6,2] * 1.6
        
        df
      })
      
      output$result10 = renderTable({
        df[1,5] = df[1,4] * 1
        df[2,5] = df[2,4] * 1
        df[3,5] = df[3,4] * 1
        df[4,5] = df[4,4] * 1
        df[5,5] = df[5,4] * 1
        df[6,5] = df[6,4] * 1
        
        df[1,6] = df[1,2] * 1.6
        df[2,6] = df[2,2] * 1.6
        df[3,6] = df[3,2] * 1.6
        df[4,6] = df[4,2] * 1.6
        df[5,6] = df[5,2] * 1.6
        df[6,6] = df[6,2] * 1.6
        
        df
      })

      df
    })
  }
)