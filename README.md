# FilePPM
----------------------------------
Image PPM P3 manipulation
(Program made for Programming Principles class)

Points to consider
* The file starts with the string P3. Although there are other variants only P3 is considered
* Lines beginning with cardinal are ignored because they are comments
* The rest of the file has only integer numbers separated by spaces or line changes. There is no practical difference between line changes and spaces, as well as there is no difference between one or more spaces or line changes

## To Compile

Run Command:

	ghc --make ExecuteMain.hs -o <output file name>
----------------------------------

## To Execute

Run Command:

	./<outputted file name> <input image name>.ppm <output image name>.ppm [flags]

Possible Flags
* -fh = horizontal Flip
* -fv = vertical Flip
* -rc = Red Filter
* -bc = Blue Filter
* -gc = Green Filter
* -gs = Black and White Filter
* -hh = decrease by half the Height
* -hw = decrease by half the Width
----------------------------------

## To Execute the Tests

Run Command:

	./<outputted file name> -t
	
