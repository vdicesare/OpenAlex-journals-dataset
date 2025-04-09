import requests
import csv
from itertools import cycle
from concurrent.futures import ThreadPoolExecutor, as_completed

def fetch_scopus_data(row, base_url, headers):
    doi = row['random_article_2023_DOI']
    row['DOI_presence_in_Scopus'] = "N"
    row["Scopus_source_ID"] = "N"
    try:
        # Get article by DOI
        response = requests.get(f"{base_url}content/search/scopus?query=DOI({doi})", headers=headers)
        if response.status_code == 200:
            data = response.json()
            entries = data.get('search-results', {}).get('entry', [])
            if entries:
                first_entry = entries[0]
                if first_entry.get('error') is None:
                    source_id = first_entry.get('source-id')
                    row["DOI_presence_in_Scopus"] = "Y"
                    if source_id is not None:
                        row["Scopus_source_ID"] = source_id
    except requests.RequestException as e:
        print(f"Error fetching DOI data for {doi}: {e}")
    return row

def main():
    base_url = "https://api.elsevier.com/"
    
    # Makes a cycle of API keys to avoid request limits with the concurrent code
    api_keys = ["d00c5546411749d36a8ddece1632d75e", "062e28247ead868289eb49c15b5ed4aa",
                "6a30c3d6bfa3ebe72599d3bd6e8576bf", "20fc3c335e60b94d1dc4773d18e1f60b",
                "2f9cb1679f063736a1c3930653c0b10a", "7f934ba44af4edf7b7eadd42ac801a34",
                "80bf02d860e2f88279a12f52b1a44797", "f69c0be695d42286e20fd82fba3cd229",
                "26b1d3605b06b7122a09d8507ea61bb1"] 
    api_key_cycle = cycle(api_keys)
    
    # Names the input file and output file
    input_file = 'sources_with_doi.csv'
    output_file = 'sources_with_doi_scopus.csv'

    # Opens the input file as inp and the output file as out
    with open(input_file, 'r') as inp, open(output_file, 'w', newline='') as out:
        input_csv = csv.DictReader(inp)
        fieldnames = ['id','OA_ID','OA_source_ID','OA_ISSN_codes','OA_journal_name',
                      'OA_journal_name_variants','OA_total_articles','random_article_2023_DOI',
                      'DOI_presence_in_Scopus','Scopus_source_ID','DOI_presence_in_WOS','WOS_source_ID']
        output_csv = csv.DictWriter(out, fieldnames=fieldnames)
        output_csv.writeheader()
        
        # Opens ThreadPoolExecutor with maximum workers 20
        with ThreadPoolExecutor(max_workers=20) as executor:
            futures = {}
            for row in input_csv:
                # Get the next API key in the cycle and update header accordingly
                api_key = next(api_key_cycle)  
                headers = {
                    "X-ELS-APIKey": api_key,
                    "Accept": "application/json"
                }
                # Submit task and store the future
                futures[executor.submit(fetch_scopus_data, row, base_url, headers)] = row
            
            # Process each completed request as it finishes
            for future in as_completed(futures):
                try:
                    result = future.result()
                    output_csv.writerow(result)
                except Exception as e:
                    print(f"Error processing row: {e}")

if __name__ == "__main__":
    main()
