!pip install requests beautifulsoup4

import requests
from bs4 import BeautifulSoup
import os
import pandas as pd

# URL of the webpage
url = "https://ignacio56.blogspot.com/2023/11/symphyotrichum-salignus.html"

# Send a GET request to the webpage
response = requests.get(url)

# Check if the request was successful
if response.status_code == 200:
    # Parse the HTML content of the webpage
    soup = BeautifulSoup(response.content, 'html.parser')

    # Find the section where the species names are listed
    species_section = soup.find('h2', text="Especies").find_next('div', class_='widget-content list-label-widget-content')

    # Extract all the species names from the section
    species_names = []
    if species_section:
        for item in species_section.find_all('a'):  # Assuming species names are in <a> tags
            species_names.append(item.text.strip())

    # Print the extracted species names
    for name in species_names:
        print(name)
else:
    print(f"Failed to retrieve the webpage. Status code: {response.status_code}")



import re

species = species_names[1]
results = []

for species in species_names:
    print(species)  # Print species for debugging

    # Construct the URL with proper encoding
    species_url = f"https://ignacio56.blogspot.com/search/label/%3A%3A{species.replace(' ', '%20')}"
    species_url = species_url.replace("::", "")  # Removing "::" if needed

    try:
        # Send GET request
        species_response = requests.get(species_url, timeout=5)  # Add timeout for reliability

        # Check if the request was successful
        if species_response.status_code == 200:
            # Parse the HTML content
            species_soup = BeautifulSoup(species_response.content, "html.parser")
            text = species_soup.get_text()

            # Search for keywords related to invasiveness
            match = re.search(r"naturaliz\w*|invasor\w*", text, re.IGNORECASE)
            if match:
                results.append({"Species": species, "Word Found": match.group()})
        else:
            print(f"Failed to retrieve the webpage for {species}. Status code: {species_response.status_code}")

    except requests.exceptions.RequestException as e:
        print(f"Error fetching data for {species}: {e}")

# Create and save the DataFrame if results exist
if results:
    df = pd.DataFrame(results)
    df.to_csv("species_search_results.csv", index=False)
    print("Search results have been saved to species_search_results.csv.")
else:
    print("No matches found.")
