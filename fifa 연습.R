fifa<-read.csv(file.choose())
fifa$Height_cm<-as.numeric(substring(fifa$Height, 1, regexpr("'",fifa$Height)-1))*30+as.numeric(substring(fifa$Height,regexpr("'",fifa$Height)+1,nchar(fifa$Height)))*2.5

regexpr("'","5'7")-1

nchar('hi')

str(fifa)
head(fifa)
table(fifa$Position)

f<-c("ST","LS","RS","LW","LF","CF","RF","RW")
?within

fifa<-within(fifa,{
      Position_Class=character(0)
      Position_Class[Position=="ST"]="Forward"
      Position_Class[Position=="LS"]="Forward"
      Position_Class[Position=="RS"]="Forward"
      Position_Class[Position=="LW"]="Forward"
      Position_Class[Position=="LF"]="Forward"
      Position_Class[Position=="CF"]="Forward"
      Position_Class[Position=="RF"]="Forward"
      Position_Class[Position=="RW"]="Forward"
      Position_Class[Position=="LAM"]="Midfielder"
      Position_Class[Position=="CAM"]="Midfielder"
      Position_Class[Position=="RAM"]="Midfielder"
      Position_Class[Position=="LM"]="Midfielder"
      Position_Class[Position=="LCM"]="Midfielder"
      Position_Class[Position=="CM"]="Midfielder"
      Position_Class[Position=="RCM"]="Midfielder"
      Position_Class[Position=="RM"]="Midfielder"
      Position_Class[Position=="LWB"]="Defender"
      Position_Class[Position=="LDM"]="Defender"
      Position_Class[Position=="CDM"]="Defender"
      Position_Class[Position=="RDM"]="Defender"
      Position_Class[Position=="RWB"]="Defender"
      Position_Class[Position=="LB"]="Defender"
      Position_Class[Position=="LCB"]="Defender"
      Position_Class[Position=="CB"]="Defender"
      Position_Class[Position=="RCB"]="Defender"
      Position_Class[Position=="RB"]="Defender"
      Position_Class[Position=="GK"]="Goalkeeper"})

str(fifa)
head(fifa)

#명목형과 순서형은 가급적 factor형으로 형변환하여 처리한다.

table(fifa$Position_Class)
pie(table(fifa$Position_Class))
barplot(table(fifa$Position_Class))

fifa$Position_Class<-factor(fifa$Position_Class, levels=c("Forward","Midfielder","Defender","Goalkeeper"),labels = c("Forward","Midfielder","Defender","Goalkeeper"))
str(fifa$Position_Class)
head(fifa)


#분산분석 (analysis of variance=anova)
#일원배치 분산분석

aov(Value ~ Position_Class, data = fifa)
fifa_result<-aov(Value ~ Position_Class, data = fifa)
summary(fifa_result)

#df(degree of freedom)자유도 : n-1
#결론 : 네가지 포지션에 따른 선수의 시장가치가 모두 동일하지 않다고 결론
# 포지션 별 선수의 시장가치(value)의 평균값들 중에서 적어도 어느 하나의 포지션은 통계적으로 유의미한 차이가 있는 것을 가진다고 말할 수 있다.


TukeyHSD(fifa_result)

#Midfielder-Forward의 p값이 0.05보다 크므로 유의미하지 않다.
#(귀무가설을 기각하지 않는다. value의 차이가 없다.)



#이원배치 분산분석

table(fifa$Preferred_Foot)

aov(Value ~ Preferred_Foot+Position_Class + Preferred_Foot:Position_Class, data = fifa)
#교호작용(서로 영향을 미치는 경우) 추가 Preferred_Foot:Position_Class
result<-aov(Value ~ Preferred_Foot+Position_Class+ Preferred_Foot:Position_Class, data = fifa)
summary(result)

TukeyHSD(result)

colnames(fifa)

nations<-names(table(fifa$Nationality))

table(fifa$Nationality)

fifa$Nationality<-factor(fifa$Nationality, levels = nations, labels = nations)

fifa$Nationality

cor(fifa$Age,fifa$Value)
cor(fifa$Overall,fifa$Value)
cor(fifa$Jersey_Number,fifa$Value)
cor(fifa$Height_cm,fifa$Value)
cor(fifa$Wage,fifa$Value)
cor(fifa$Weight_lb,fifa$Value)


#문제 : Age,Overall,wage,Height_cm,Weight_lb가 Value에 영향을 미치는지 알아보는 회귀분석
#단계적 선택법을 사용하고 결과를 해석하시오.
step(lm(Value ~ 1,data=fifa), scope=list(lower=~1,upper=~Age+Overall+Wage+Height_cm+Weight_lb), direction="both")
#모델이 손상하는 정보가 적을수록 해당 모델의 품질이 높아진다.

fifa.lm<-lm(Value ~ Wage + Overall + Age + Height_cm, data = fifa)
summary(fifa.lm)
