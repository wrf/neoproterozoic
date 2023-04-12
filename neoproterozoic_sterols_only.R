

maxy = 10

pdf(file="~/git/neoproterozoic/neoproterozoic_sterols_only_plot.pdf", width=7, height=6)
#png(file="~/git/neoproterozoic/neoproterozoic_plot.png", width=800, height=700)

par(mar=c(4.5,5,1,0.5))


plot(0,0,type='n',xlim=c(-1000,-490),ylim=c(0,maxy),axes=FALSE,frame.plot=FALSE, xlab="Time (Ma)", ylab="", cex.lab=1.3)
axis(1,at=seq(-1000,-500,100),labels=rev(seq(500,1000,100)),cex.axis=1.4)

#snowball earths
rect(-717, 0, -659, maxy, col="#8e8eae99", border=FALSE)
rect(-646, 0, -635, maxy, col="#8e8eae99", border=FALSE)


### draw geological periods
period_letters = c("Tonian","Cr","Ed","C","O","S","D","C","P","T","J","K","Pe","N")
period_starts =  c(-1000,-720,-635,-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23)
period_ends = c(-720,-635,-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23,0)
period_color = c("#c6c6c6", "#63a9c3", "#d6ab24", "#6da26e", "#28a46e", "#a5e3e5", "#d48f16", "#4fa092", "#d9491c", "#be58d0", "#4d94de", "#26bf3f", "#dba426", "#baab09")
rect(period_starts[1:4],-0.38,period_ends[1:4],-0.1, border="#FFFFFF", col=period_color)
text( (period_starts[1:4]+period_ends[1:4])/2, -0.225, period_letters[1:4])
text( -516, -0.225, "-", cex=1.1) # add dash for Cambrian


### DRAW S/H ratios
#mtext("A",side=2,las=1,cex=2, at=3.6,line=3.3)
brocksdata = read.table("~/git/neoproterozoic/brocks2017_ext_table_2.tab", header=TRUE, sep="\t")
has_sh = !is.na(brocksdata[["S.H"]])
brocks_age = brocksdata[,5][has_sh]
brocks_sh = brocksdata[,11][has_sh]
brocks_sh
axis(2, at=seq(5.2,10,0.8), labels=c(0,"",1,"",2,"",3), cex.axis=1.3)
mtext("Sterane/Hopane", side=2, at=7.6, line=2.5, cex=1.5)
lines(c(-1000,-500),c(5.2,5.2),lwd=0.3)
points(-brocks_age,brocks_sh*1.6+5.2, pch=15, cex=2, col="#2690bc99")
legend(-980,9.5,legend=c("Sterane/hopane ratio"), pch=15, pt.cex=3, cex=1.4, col=c("#2690bc"), box.lwd=0.5 )

#mtext("B",side=2,las=1,cex=2, at=2.2,line=3.2)
has_steranes = (!is.na(brocksdata[["steranes"]])) & (!brocksdata[["steranes"]]=="0_0_0")

c27_conc = as.numeric(unlist(lapply(strsplit(as.character(brocksdata[["steranes"]])[has_steranes],"_"), function(x) x[1])))
c28_conc = as.numeric(unlist(lapply(strsplit(as.character(brocksdata[["steranes"]])[has_steranes],"_"), function(x) x[2])))
c29_conc = as.numeric(unlist(lapply(strsplit(as.character(brocksdata[["steranes"]])[has_steranes],"_"), function(x) x[3])))

axis(2, at=c(0,2.1,4.2), labels=c(0,0.5,1), cex.axis=1.4)
mtext("Relative abundance", side=2, at=2.1, line=2.5, cex=1.5)
lines(c(-1000,-500),c(0.0,0.0),lwd=0.3)
points(-brocks_age[has_steranes],c27_conc/(100/4.2), pch=18, cex=3, col="#cd0f3299")
points(-brocks_age[has_steranes],c28_conc/(100/4.2), pch=18, cex=3, col="#470fcd99")
points(-brocks_age[has_steranes],c29_conc/(100/4.2), pch=18, cex=3, col="#0fcd4b99")

brockscambdata = read.table("~/git/neoproterozoic/brocks2017_supp_tab1_cambrian_only.tab", header=TRUE, sep="\t")
has_sh = !is.na(brockscambdata[["S_H"]])
brocks_age = brockscambdata[["appx_Ma"]][has_sh]
brocks_sh = brockscambdata[["S_H"]][has_sh]
points(-brocks_age,brocks_sh*1.6+5.2, pch=15, cex=2, col="#2690bc99")

has_steranes = !is.na(brockscambdata[["C27"]])
points(-brocks_age[has_steranes],brockscambdata[["C27"]][has_steranes]/(100/4.2), pch=18, cex=3, col="#cd0f3299")
points(-brocks_age[has_steranes],brockscambdata[["C28"]][has_steranes]/(100/4.2), pch=18, cex=3, col="#470fcd99")
points(-brocks_age[has_steranes],brockscambdata[["C29"]][has_steranes]/(100/4.2), pch=18, cex=3, col="#0fcd4b99")

legend(-980,4.2,legend=c("C27","C28","C29"), pch=18, pt.cex=3, cex=1.4, col=c("#cd0f32","#470fcd","#0fcd4b"), box.lwd=0.5)


dev.off()

#