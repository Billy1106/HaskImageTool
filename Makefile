clean:
	rm -rf outputs/*

resize_demo:
	cabal run HaskImageTool -- resize width=800 height=600 --input=inputs/cat.jpg --output=outputs/resized_cat.png
