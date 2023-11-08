import sys, requests, argparse, base64, os.path
import pandas as pd

parser = argparse.ArgumentParser()
parser.add_argument(
    "--output", "-o",
    type = str,
    dest = "output",
    default = "rMLST_test.tsv",
    help = "Output tsv file path."
)
parser.add_argument(
  '--file_list', '-f',
  type = str,
  nargs = '+',
  default = [
    "/srv/data/BIG/bifrost/2022/220902_NB551234_0532_N_WGS_600_AHTC5WAFX3/AMA004611/assemblatron/AMA004611.fasta",
    "/srv/data/BIG/bifrost/2022/220802_NB551234_0523_N_WGS_591_AHT5HNAFX3/AMA004526/assemblatron/AMA004526.fasta"
  ],
  help='assembly contig filename (FASTA format)')
args = parser.parse_args()


def main(assembly_file):
  sample = os.path.basename(assembly_file).rstrip("\.fasta")
  print(sample)
  
  uri = 'http://rest.pubmlst.org/db/pubmlst_rmlst_seqdef_kiosk/schemes/1/sequence'
  with open(assembly_file, 'r') as x: 
    fasta = x.read()
  print(" - Encoding and uploading fasta", flush = True)
  payload = '{"base64":true,"details":true,"sequence":"' + base64.b64encode(fasta.encode()).decode() + '"}'
  response = requests.post(uri, data=payload)
  print(" - Delineating response", flush = True)
  if response.status_code == requests.codes.ok:
    data = response.json()
    try: 
      data['taxon_prediction']
    except KeyError:
      print(" - No rMLST match!", flush = True)
      sys.exit(0)

    match = pd.DataFrame(columns = ["Sample", "Match", "Species", "Rank", "Percentage"])

    for results in data['taxon_prediction']:
      taxon = results['taxon']
      genus = taxon[0]
      family = taxon.split()[-1]
      species = "%s. %s" %(genus, family)

      match.loc[len(match.index)] = [sample, taxon, species, results['rank'], results['support']]

    return(match)

  else:
    print(response.text, flush = True)


if __name__ == "__main__":
  assembly_list = args.file_list

  matches = list()
  rmlst = pd.DataFrame()

  for assembly_file in assembly_list:

    match = main(assembly_file)

    if match is not None:
      matches.append(match)

  rmlst = pd.concat(matches)

  output_path = args.output
  rmlst.to_csv(output_path, sep = "\t", index = False)
  print("Success!", flush = True)