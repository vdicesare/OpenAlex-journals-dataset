import requests
import csv
import argparse

# Requires: issn of journal, base url of API, and headers for API request
# Effects: 
#   1. Returns WOS source id of journal if successful
#   2. Returns -1 if there is an error fetching source id from DOI
#   3. Returns -2 if there is an error fetching journal info from DOI
def get_journal_by_issn(issn, base_url, headers):
    response = requests.get(f"{base_url}/journals?issn={issn}", headers=headers)
    journal_data = response.json()
    journal_info = journal_data.get('hits',[])
    if journal_info:
        source_id = journal_info[0].get('id')
        if source_id:
            return source_id
        else:
            return -1
    else:
        return -2


def main(start_row,end_row,file_number):

    base_url = "https://api.clarivate.com/apis/wos-starter/v1/"
    headers = {
        "X-APIKey": "3d48afe30b957f45fcc3202af69e3149f308576f",
        "Accept": "application/json"
    }

    with open('sources_with_doi_scopus.csv','r') as inp, open(f'sources_with_wos_{file_number}.csv', 'w') as out:
        input_csv = csv.DictReader(inp)
        fieldnames = ['id','OA_ID','OA_source_ID','OA_ISSN_codes','OA_journal_name',
                      'OA_journal_name_variants','OA_total_articles','random_article_2023_DOI',
                      'DOI_presence_in_Scopus','Scopus_source_ID','DOI_presence_in_WOS','WOS_source_ID']
        output_csv = csv.DictWriter(out, fieldnames=fieldnames)
        output_csv.writeheader()
        
        # For each row [start_row, end_row) (zero indexed, excluding header)
        for i, row in enumerate(input_csv):
            if i < start_row:
                continue
            if end_row is not None and i >= end_row:
                break
                
            # Get article by DOI
            doi = row['random_article_2023_DOI'].replace("https://doi.org/", "")
            response = requests.get(f"{base_url}/documents?q=DO={doi}", headers=headers)

            # Initialize values in CSV row
            row['DOI_presence_in_WOS'] = "N"
            row['WOS_source_ID'] = "N"

            if response.status_code == 200:
                data = response.json()
                entries = data.get('hits', [])
                if entries:
                    first_entry = entries[0]

                    # Checks if the DOI is found and updates CSV accordingly
                    if first_entry.get('error') is None:
                        is_found = False
                        issn = first_entry.get('identifiers', {}).get('issn')
                        eissn = first_entry.get('identifiers', {}).get('eissn')
                        row["DOI_presence_in_WOS"] = "Y"

                        # Looks up source id by issn, updates is_found to true
                        if issn:
                            source_id = get_journal_by_issn(issn, base_url, headers)
                            if source_id == -1:
                                print(f"Error fetching source_id for {doi} by issn")
                            elif source_id == -2:
                                print(f"Error fetching journal info for {doi} by issn")
                            else:
                                is_found = True
                                row['WOS_source_ID'] = source_id

                        # Looks up source id by eissn, updates is_found to true
                        if eissn and not is_found:
                            source_id = get_journal_by_issn(eissn, base_url, headers)
                            if source_id == -1:
                                print(f"Error fetching source_id for {doi} by eissn")
                            elif source_id == -2:
                                print(f"Error fetching journal info for {doi} by eissn")
                            else:
                                is_found = True
                                row['WOS_source_ID'] = source_id

                        # Prints that there is an error if neither the issn or the eissn lookups are successful
                        if not is_found:
                            print(f"Error fetching issn for {doi}")
            else:
                print(f"Error fetching DOI data for {doi}: {response}")
            print(row["OA_source_ID"] + ": " + row["DOI_presence_in_WOS"] + " " + row["WOS_source_ID"])
            output_csv.writerow(row)


if __name__ == "__main__":
    # python3 __wos__.py <start_row> <end_row> <file_number> > error<file_number>.txt
    # ej. "python3 __wos__.py 1000 2000 2 > error2.txt" will create sources_with_wos_2.csv 
    #   that contains the output from row 1000 to 1999, and error2.txt which contains the
    #   command line output as well as the errors encountered


    # Set up command line arguments
    parser = argparse.ArgumentParser(description='Process specified range of CSV rows.')
    parser.add_argument('start_row', type=int, nargs='?', default=0, help='The starting row index (inclusive)')
    parser.add_argument('end_row', type=int, nargs='?', help='The ending row index (exclusive)')
    parser.add_argument('file_number', type=int, nargs='?', default=0, help='The number of the output file')

    args = parser.parse_args()

    if args.end_row is None:
        with open('sources_with_doi_scopus.csv', 'r') as f:
            total_lines = sum(1 for line in f) - 1  # subtract 1 for header
        args.end_row = total_lines


    main(args.start_row, args.end_row, args.file_number)