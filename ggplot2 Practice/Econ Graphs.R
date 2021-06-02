# ECO 305 Final graphs

# Figure 1: The Vacancy Graph.
library(ggplot2)
library(ggfortify)
vacancy = read.csv("Job Opening.csv", header = T)
vacancy$DATE <- regmatches(vacancy$DATE, regexpr("20\\d\\d-\\d\\d", vacancy$DATE))

vacancy_graph <- ggplot(vacancy, aes(x=DATE, y=JTSJOL, group=1)) + ylab("Job Openings") + annotate(geom="vline", x="2020-02", xintercept="2020-02", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2020-02", y=7750, label="Pandemic Recession Start", vjust=1.4, size=4, fontface=3, angle=15) + geom_path(colour = "#D34F73") + geom_point() + ggtitle("Number of Job Openings", "Level in Thousands, Seasonally Adjusted") + theme(axis.title.x = element_text(face="bold", size=14, colour = "#A37B73"),
                                                                                       axis.text.x = element_text(face="bold", angle = 45, colour="#3F292B"),
                                                                                       axis.title.y = element_text(face="bold", size=14, colour = "#A37B73"),
                                                                                       axis.text.y = element_text(face = "bold", colour="#3F292B"),
                                                                                       plot.title = element_text(face="bold", size=30, colour="#A37B73"),
                                                                                       plot.subtitle = element_text(size=11, colour="#cfb9b5"),)
vacancy_graph

# Figure 2: Drawing the equation (1).
base <- ggplot() + xlim(0, 100000)
base <- base + geom_function(colour="#DB7F67", fun = function(x){x*(1-(1-1/x)^1000)}) + ylab("Y") + xlab("X")
base <- base + ggtitle("Equation (1)\n")
base <- base + theme(axis.title.x = element_text(face="bold", size=14, colour = "#A37B73"),
                     axis.text.x = element_text(face="bold", colour="#3F292B"),
                     axis.title.y = element_text(face="bold", size=14, colour = "#A37B73"),
                     axis.text.y = element_text(face = "bold", colour="#3F292B"),
                     plot.title = element_text(face="bold", size=30, colour="#A37B73"))
base

# Figure 3: Vacancy & The Finding Rate.
finding = read.csv("Finding Rates.csv", header=T)
finding <- finding[c(1,4)]
finding <- finding[-1,]
finding$DATE <- regmatches(finding$DATE, regexpr("20\\d\\d/\\d\\d", finding$DATE))
finding$DATE <- sub("/","-",finding$DATE)

vacancy$GROUP <- "Job Openings, Level in Thousands"
names(vacancy)[names(vacancy)=="JTSJOL"] <- "Figures"
finding$GROUP <- "Finding Rates * 20,000"
names(finding)[names(finding)=="FindingRate"] <- "Figures"
finding[2] <- finding[2]*20000
vacancy_and_finding <- rbind(vacancy, finding)
vacancy_and_finding$GROUP <- factor(vacancy_and_finding$GROUP)

vandf_graph <- ggplot(vacancy_and_finding, aes(x=DATE, y=Figures, group=GROUP, colour=GROUP, shape=GROUP)) + geom_path()+ geom_point() + scale_colour_manual(values=c("#D34F73", "#C9CBA3")) + ylab("")
vandf_graph <- vandf_graph + annotate(geom="vline", x="2020-02", xintercept="2020-02", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2020-02", y=7750, label="Pandemic Recession Start", vjust=1.4, size=4, fontface=3, angle=15)
vandf_graph <- vandf_graph + ggtitle("Job Openings and Finding Rate Multiplied By 20,000\n")
vandf_graph <- vandf_graph + theme(axis.title.x = element_text(face="bold", size=14, colour = "#A37B73"),
                                   axis.text.x = element_text(face="bold", angle = 45, colour="#3F292B"),
                                   axis.text.y = element_text(face = "bold", colour="#3F292B"),
                                   plot.title = element_text(face="bold", size=25, colour="#A37B73"),
                                   legend.text = element_text(face="bold"),
                                   legend.title = element_text(face="bold", colour="#A37B73"))
vandf_graph

# Figure 4: Transition from Employed to Unemployed and the Job Openings
transition <- read.csv("Separation Rates.csv", header = T)
transition <- transition[-1,]
transition$DATE <- regmatches(transition$DATE, regexpr("20\\d\\d/\\d\\d", transition$DATE))
transition$DATE <- sub("/","-",transition$DATE)

transition$GROUP <- "Labor Force Flows Employed to Unemployed"
vacancy$GROUP <- "Job Openings"
transition1 <- transition[-(3:4)]
names(transition1)[2] <- "Figures"
vacancy_and_transition <- rbind(vacancy, transition1)
vacancy_and_transition$GROUP <- factor(vacancy_and_transition$GROUP)

vandt_graph <- ggplot(vacancy_and_transition, aes(x=DATE, y=Figures, group=GROUP, colour=GROUP, shape=GROUP)) + geom_path() + geom_point() + scale_colour_manual(values=c("#D34F73", "#C9CBA3")) + ylab("")
vandt_graph <- vandt_graph + annotate(geom="vline", x="2020-02", xintercept="2020-02", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2020-02", y=7750, label="Pandemic Recession Start", vjust=1.4, size=4, fontface=3, angle=15)
vandt_graph <- vandt_graph + ggtitle("Job Openings and The Labor Force Flows Employed to Unemployed", "Levels in Thousands, Seasonally Adjusted")
vandt_graph <- vandt_graph + theme(axis.title.x = element_text(face="bold", size=14, colour = "#A37B73"),
                                   axis.text.x = element_text(face="bold", angle = 45, colour="#3F292B"),
                                   axis.text.y = element_text(face = "bold", colour="#3F292B"),
                                   plot.title = element_text(face="bold", size=25, colour="#A37B73"),
                                   plot.subtitle = element_text(size=11, colour="#cfb9b5"),
                                   legend.text = element_text(face="bold"),
                                   legend.title = element_text(face="bold", colour="#A37B73"))
vandt_graph

# Figure 5-1: Job Opening and Labor Force Flow: Employed to Unemployed
jolf <- read.csv("Employed to Unemployed V.S Job Opening.csv", header=T)
jolf$DATE <- regmatches(jolf$DATE, regexpr("20\\d\\d-\\d\\d", jolf$DATE))
jolf1 <- jolf[c(1,2)]
jolf2 <- jolf[c(1,3)]
names(jolf1)[2] = names(jolf2)[2] = "Figures"
jolf1$GROUP <- factor("Job Openings")
jolf2$GROUP <- factor("Labor Force Flows Employed to Unemployed")
jolf <- rbind(jolf1, jolf2)

jolf_graph <- ggplot(jolf, aes(x=DATE, y=Figures, group=GROUP, colour=GROUP, shape=GROUP)) + geom_path() + geom_point() + scale_colour_manual(values=c("#D34F73", "#C9CBA3")) + ylab("Thousands of Persons")
# Pandemic Recession Vertical Line
jolf_graph <- jolf_graph + annotate(geom="vline", x="2020-02", xintercept="2020-02", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2020-02", y=12500, label="Pandemic Recession Start", vjust=-0.6, size=5, fontface=3, angle=90)
# Second Recession Vertical Line
jolf_graph <- jolf_graph + annotate(geom="vline", x="2007-12", xintercept="2007-12", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2007-12", y=12500, label="Recession Start: December 2007", vjust=1.4, size=5, fontface=3, angle=90)
# First Recessionf Vertical Line
jolf_graph <- jolf_graph + annotate(geom="vline", x="2001-03", xintercept="2001-03", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2001-03", y=12500, label="Recession Start: March 2001", vjust=1.4, size=5, fontface=3, angle=90)
jolf_graph <- jolf_graph + ggtitle("Job Openings and The Labor Force Flows Employed to Unemployed")
jolf_graph <- jolf_graph + theme(axis.title.x = element_text(face="bold", size=14, colour = "#A37B73"),
                                 axis.title.y = element_text(face="bold", size=14, colour = "#A37B73"),
                                 axis.text.y = element_text(face = "bold", colour="#3F292B"),
                                 plot.title = element_text(face="bold", size=25, colour="#A37B73"),
                                 plot.subtitle = element_text(size=11, colour="#cfb9b5"),
                                 legend.text = element_text(face="bold"),
                                 legend.title = element_text(face="bold", colour="#A37B73"),
                                 legend.position = "bottom")
jolf_graph <- jolf_graph + scale_x_discrete(breaks=NULL) # Hiding the ticks of x-axis

jolf_graph

# Figure 5-2: Job Opening and Separation Rate
separation <- transition
separation$GROUP <- "Separation Rate * 20,000"
vacancy$GROUP <- "Job Openings, Levels in Thousands"
separation <- separation[-c(2,3)]
names(separation)[2] <- "Figures"
separation$Figures <- separation$Figures * 20000
vacancy_and_separation <- rbind(vacancy, separation)
vacancy_and_separation$GROUP <- factor(vacancy_and_separation$GROUP)

vands_graph <- ggplot(vacancy_and_separation, aes(x=DATE, y=Figures, group=GROUP, colour=GROUP, shape=GROUP)) + geom_path() + geom_point() + scale_colour_manual(values=c("#D34F73", "#C9CBA3")) + ylab("")
vands_graph <- vands_graph + annotate(geom="vline", x="2020-02", xintercept="2020-02", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2020-02", y=7750, label="Pandemic Recession Start", vjust=1.4, size=4, fontface=3, angle=15)
vands_graph <- vands_graph + ggtitle("Job Openings and The Separation Rate Multiplied By 20,000\n")
vands_graph <- vands_graph + theme(axis.title.x = element_text(face="bold", size=14, colour = "#A37B73"),
                                   axis.text.x = element_text(face="bold", angle = 45, colour="#3F292B"),
                                   axis.text.y = element_text(face = "bold", colour="#3F292B"),
                                   plot.title = element_text(face="bold", size=25, colour="#A37B73"),
                                   plot.subtitle = element_text(size=11, colour="#cfb9b5"),
                                   legend.text = element_text(face="bold"),
                                   legend.title = element_text(face="bold", colour="#A37B73"))
vands_graph

# Figure 6: Employment, Unemployment, and Labor Force

labor <- read.csv("Employment, Unemployment, and Labor Force.csv", header=T)
labor$DATE <- regmatches(labor$DATE, regexpr("20\\d\\d/\\d\\d", labor$DATE))
labor$DATE <- sub("/","-",labor$DATE)
lf <- labor[-c(3,4)]
em <- labor[-c(2,4)]
un <- labor[-c(2,3)]
lf$GROUP <- "Labor Force Level"
em$GROUP <- "Employment Level"
un$GROUP <- "Unemployment Level"
names(lf)[2] = names(em)[2] = names(un)[2] <- "Figures"
lf_em_un <- rbind(lf,em,un)

lfemun_graph <- ggplot(lf_em_un, aes(x=DATE, y=Figures, group=GROUP, colour=GROUP, shape=GROUP)) + geom_path() + geom_point() + scale_colour_manual(values=c("#D34F73", "#808349", "#F59F00")) + ylab("Thousands of Persons")
lfemun_graph <- lfemun_graph + annotate(geom="vline", x="2020-02", xintercept="2020-02", linetype="dashed", color = "#DB7F67", size=1) + annotate(geom = "text", x="2020-02", y=100000, label="Pandemic Recession Start", vjust=1.4, size=4, fontface=3, angle=15)
lfemun_graph <- lfemun_graph + ggtitle("Labor Force, Employment, and Unemployment Levels\n")
lfemun_graph <- lfemun_graph + theme(axis.title.x = element_text(face="bold", size=14, colour = "#A37B73"),
                                    axis.text.x = element_text(face="bold", angle = 45, colour="#3F292B"),
                                    axis.title.y = element_text(face="bold", size=14, colour = "#A37B73"),
                                    axis.text.y = element_text(face = "bold", colour="#3F292B"),
                                    plot.title = element_text(face="bold", size=25, colour="#A37B73"),
                                    plot.subtitle = element_text(size=11, colour="#cfb9b5"),
                                    legend.text = element_text(face="bold"),
                                    legend.title = element_text(face="bold", colour="#A37B73"))
lfemun_graph

# Figure 7: AD, LRAS, and SRAS Curves
SRAS <- data.frame(x=c(0,10),y=c(5,5))
LRAS <- data.frame(x=c(5,5),y=c(0,10))
curves <- ggplot()
curves <- curves + geom_function(fun = function(x) 1/(x-4))
curves
curves <- curves + geom_line(data = SRAS, aes(x=x,y=y))
curves <- curves + geom_line(data = LRAS, aes(x=x,y=y))
curves













