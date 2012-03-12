August 10, 2011		- Bill Galbraith and Alan Teeder

These files represent the changes made to the original code, to correct

	* latent code errors
	* issues that have arisen from the use of more modern compilers 
 	  such as the Intel F90 Visual Fortran compiler.

The changes below are indicated as "latent" or "compiler". These compiler
issues do not appear in the version translated with F2C and compiled
as C code under Cygwin or Visual C++ 2008, only under a more modern version
such as the Intel F90 Visual Fortran compiler.

-----------------------------------------------------------------------

CONV.F near lines 81 and 123	- compiler issue

Two fixes to prevent array boundary overruns in modern compilers.

-----------------------------------------------------------------------

PRPWEF.F	- latent problems (all)

Change 1 - In common block POWR, near line 41, EPOWR is declared as 
an array. It is not supposed to be. This also shows up near 
line 590 and 591.


Change 2 - Near line 320 is this function call: (this is correct)

      CALL TLINEX(X14161,X24161,Y44161,4,10,AR,XBARP/CRP,DEUDA,
     1            2,2,2,2,Q44161,2,ROUTID)

Some versions have a wrong parameter in the function call (note that
XBARP should be divied by CRP in the call):

      CALL TLINEX(X14161,X24161,Y44161,4,10,AR,XBARP,DEUDA,
     1            2,2,2,2,Q44161,2,ROUTID)



Change 3 - Near line 504 is the line

         BIO2=SQRT(PRPRD2-(ZS-ZW)**2)

There are times when this ends up being a square root of a negative
number. This occurs in the original executable code. One solution is
to ensure this never goes negative. There did not appear to be any 
difference if this were just set to 0.0 either.

         BIO2=SQRT(ABS(PRPRD2-(ZS-ZW)**2))

To verify these issues, use the enclosed EXAMPLE1.IN file, which also
has the EXAMPLE1.OUT answer. This model is for the T-34C, but it is
believed that any propeller-powered model with any AOA values above 
about 14 degs will yield an incorrect answer, or NaN values (or other 
indications depending on the compiler that the number is not value).

-----------------------------------------------------------------------

SUBRYW.F	- compiler issue

Near lines 33 (1 line) and 401 (9 lines), this prevents some array
overruns.

-----------------------------------------------------------------------

TBFUNX.F 	- latent problem

Near line 49, L is initialized to 1, to prevent an array out of bounds 
near line 84. Compiler issue.

Near line 53, there is an issue in find in the min and max values 
of an array. This fix solves that problem.

To verify these issues, use the enclosed EXAMPLE1.IN file, which also
has the EXAMPLE1.OUT answer. This model is for the T-34C, but it is
believed that any model with 20 AOA values will yield an incorrect 
answer, or NaN values (or other indications depending on the compiler 
that the number is not value).

-----------------------------------------------------------------------

TLIP1.F		- compiler issue

Near line 5. These dimension changes are required as  TLIP1X.F is 
called from suppah.f and suppaw.f with dimensions not compatible 
with those in the original.

-----------------------------------------------------------------------

WBCM.F		- compiler issue

Near lines 13 and 17, these arrays are redimensioned to (1) to prevent
an overagressive compiler such as the Intel F90 compiler from throwing
an error near line 109

         XAC=-BODY(J+120)*CBARR/BODY(J+100)+DXCG

for the case where there NALPHA=20. With a compiler such as the F2C 
translator that is then compiled as C code, this is not a problem.

-----------------------------------------------------------------------

WBDRAG.F	- compiler issue

Near line 8, these arrays are rediemnsioned to (1), similar to WBCM.F
above, where the problem is BD(J+214)

 

