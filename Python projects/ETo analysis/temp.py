sub1 = "python string!"
sub2 = "an arg"

a = "i am a %s"%sub1
b = "i am a {0}".format(sub1)

c = "with %(kwarg)s!"%{'kwarg':sub2}
d = "with {kwarg}!".format(kwarg=sub2)

print (a)
print (b)
print (c)
print (d)


'Total with tax: $%.2f' % (13.00 * 1.0825) 
#'Total with tax: $14.07'

print('Total with tax: ${:09.4f}'.format(13.00 * 1.0825))
#Total with tax: $0014.0725
print('Total with tax: ${: 9.4f}'.format(13.00 * 1.0825))
#Total with tax: $  14.0725

##
##        import re
##        import textwrap
##
##        a = [  5.50056103e+02,   6.77383566e+03,   6.01001513e+05,
##                 3.55425142e+08,   7.07254875e+05,   8.83174744e+02,
##                 8.22320510e+01,   4.25076609e+08,   6.28662635e+07,
##                 1.56503068e+02]
##
##        thelist = textwrap.dedent(
##                '\n'.join(ut0.sub(r'\1', "%20f" % x) for x in a)).splitlines()
##
##        print '\n'.join(thelist)
##
##        emits:
##
##              550.056103
##             6773.83566
##           601001.513
##        355425142.0
##           707254.875
##              883.174744
##               82.232051
##        425076609.0
##         62866263.5
##              156.503068
