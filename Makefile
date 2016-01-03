hcast:
	ghc -o hcast hcast.hs app.hs camera.hs color.hs geometry.hs image.hs light.hs math.hs object.hs platonic.hs scene.hs screen.hs shape.hs 

run:
	./hcast --output output.ppm --height 100 --width 100

clean:
	rm *.exe *.hi *.o *.ppm
