# Banner Creation Process

Created custom banner images for the repository README to enhance visual appeal and clearly communicate the project purpose.

## Process Details
1. Generated multiple image options using text-to-image models with the following prompt:
```
/imagine Create a vibrant, engaging header image for a GitHub repository about a 5-Day Generative AI course with Google's Gemini models. The image should:
1. Feature a modern, colorful design with light background suitable for both light and dark mode interfaces
2. Include visual elements representing AI learning, Python coding, and generative models
3. Have a mix of abstract neural network patterns and concrete symbols
4. Include the text "5-Day Gen AI Intensive" in a clean, modern font
5. Use a color palette with blues, purples, and teals (Google AI colors)
6. Have a 16:5 aspect ratio for optimal display in GitHub README
7. Include subtle geometric patterns or code-like elements in the background
8. Feature a progression theme suggesting the 5-day learning journey
9. Have a professional but approachable style suitable for developers
10. Include small visual elements representing daily course topics
```

2. Saved raw generated images to `images/wip/splash/` directory
3. Renamed most promising images with descriptive names:
   - network-gradient-purple-teal.jpeg
   - circular-network-dark.jpeg
   - horizontal-banner-gradient.jpeg
   - horizontal-banner-timeline.jpeg
   - wave-pattern-blue-purple.jpeg
   - pixel-art-network.png

4. Created optimized banner versions:
   - Used horizontal-banner-gradient.jpeg as main banner (1024x340)
   - Created subtle banner from wave-pattern-blue-purple.jpeg (1280x160) using:
   ```
   convert images/wip/splash/wave-pattern-blue-purple.jpeg -crop 1280x160+0+400 -quality 90 images/banner-subtle.jpg
   ```

5. Added banners to README.org:
   - Main banner at top of document
   - Subtle banner in resources section

## Next Steps
- [ ] Optimize banner file sizes further if needed
- [ ] Consider additional banner locations in documentation 
- [ ] Create specific day-by-day banners for course notebooks