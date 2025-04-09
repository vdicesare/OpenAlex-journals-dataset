import requests
import csv

def main():
    base_url = "https://api.elsevier.com/"
    headers = {
        "X-ELS-APIKey": "7f934ba44af4edf7b7eadd42ac801a34",
        "Accept": "application/json"
    }
    input_file = 'final_out.csv'
    output_file = 'scopus_out.csv'

    with open(input_file,'r') as inp, open(output_file, 'w') as out:
        # Creates DictReader to manipulate the csv files
        input = csv.DictReader(inp)
        fieldnames = ['OA ID','OA source ID','ISSN codes','Journal title','Total articles','articles x journal','articles x journal acum','Random article DOI from 2023','Presence of DOI in Scopus','Scopus source ID']
        output = csv.DictWriter(out, fieldnames=fieldnames)

        # Iterate through each line in input csv
        for row in input:
            doi = row['Random article DOI from 2023'].replace("https://doi.org/", "")

            # Get article by DOI
            response = requests.get(f"{base_url}content/search/scopus?query=DOI({doi})", headers=headers)

            # Initialize values we are trying to find to N
            row['Presence of DOI in Scopus'] = "N"
            row["Scopus source ID"] = "N"

            # If the request is successful get the values returned and fill the values in
            if response.status_code == 200:
                data = response.json()
                entries = data.get('search-results', {}).get('entry', [])
                if entries:
                    first_entry = entries[0]
                    if first_entry.get('error') is None:
                        source_id = first_entry.get('source-id')
                        row["Presence of DOI in Scopus"] = "Y"
                        if source_id is not None:
                            row["Scopus source ID"] = source_id
            print(row["OA source ID"] + ": " + row["Presence of DOI in Scopus"] + " " + row["Scopus source ID"])
            output.writerow(row)


if __name__ == "__main__":
    main()