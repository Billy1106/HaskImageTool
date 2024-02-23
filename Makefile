clean:
	rm -rf outputs/*

resize_demo:
	cabal run HaskImageTool -- resize --width=800 --height=600 --input=inputs/cat.jpg --output=outputs/resized_cat.png

mosaic_demo:
	cabal run HaskImageTool -- mosaic --size=10 --input=inputs/cat.jpg --output=outputs/mozaic_image.png

edge_detection_demo:
	cabal run HaskImageTool -- edge-detection --input=inputs/cat.jpg --output=outputs/edge_detection_image.png