###############################################################################
#
# Makefile for gateToOpus
#
###############################################################################
#
# User defined options
#
###############################################################################

INSTALLDIR = /home/ctuser_jw/skill/

###############################################################################
#
# Variables
#
###############################################################################

srcs = gateToOpus.c
objs = gateToOpus.o

###############################################################################
#
# Targets
#
###############################################################################

gateToOpus: $(objs)
	$(CC) $(objs) -o gateToOpus

install: gateToOpus
	cp gateToOpus gateToOpus.ksh gateToOpus.el ${INSTALLDIR}
#	cpp -E gateToOpus.il -DINSTALLDIR=\"${INSTALLDIR}\" > \
#	${INSTALLDIR}/gateToOpus.il
	cpp -P -DINSTALLDIR=\"${INSTALLDIR}\" gateToOpus.il ${INSTALLDIR}/gateToOpus.il
