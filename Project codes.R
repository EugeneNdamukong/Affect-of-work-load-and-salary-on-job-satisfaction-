WS.dat=read.table("C:/Users/Ambe/Desktop/First Semester/Analysis of Variance/sleep.txt",header = TRUE)
attach(WS.dat)
names(WS.dat)
unique(WS.dat$salary)
unique(WS.dat$wl)
interaction.plot(WS.dat$wl,WS.dat$salary,WS.dat$y,xlab = 'Work load',ylab = 
                   'Work score')
dum.mat=model.matrix(~salary*wl,y,
                     data=WS.dat,contrasts=list(wl='contr.sum',
                                                salary='contr.sum'))
dum.mat=as.data.frame(dum.mat)
empty.fit=lm(y~1,data=dum.mat)
Salary.fit=lm(y~salary1,data=dum.mat)
Salary.WL.fit=lm(y~salary1+wl1 + wl2,data=dum.mat)
full.fit=lm(y~salary1+wl1 + wl2+salary1:wl1+
            salary1:wl2,data=dum.mat)
anova(empty.fit,Salary.fit,Salary.WL.fit,full.fit)
