work_f="/home/rstudio/RNAseq/"
threadN=12
species="Zebrafish"
ncbi_f=${work_f}"database/"${species}"/"
cd $ncbi_f
project_f=${work_f}"221211_Rit_zebra/"
read_pair="pair"



# anotationデータの取得 なんか，ftpサーバーから落とせないのでhttpsから落としてしまう
# "ftp://ftp.ensembl.org/pub/release-106/fasta/mus_musculus/cdna/Mus_musculus.GRCm39.cdna.all.fa.gz"
wget "https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/002/035/GCF_000002035.6_GRCz11/GCF_000002035.6_GRCz11_genomic.fna.gz"
wget "https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/002/035/GCF_000002035.6_GRCz11/GCF_000002035.6_GRCz11_genomic.gtf.gz"

gzip -d GCF_000002035.6_GRCz11_genomic.fna.gz
gzip -d GCF_000002035.6_GRCz11_genomic.gtf.gz

fna_f="GCF_000002035.6_GRCz11_genomic.fna"
gtf_f="GCF_000002035.6_GRCz11_genomic.gtf"

star_f=${work_f}"star_index/"${species}"/"

STAR --runMode genomeGenerate \
--genomeDir ${star_f}"GRCz11" --runThreadN $threadN \
--genomeFastaFiles $fna_f \
--sjdbGTFfile $gtf_f


rsem_f=${work_f}"rsem_index/"${species}"/"
cd $rsem_f

rsem-prepare-reference --gtf ${ncbi_f}${gtf_f} \
-p $threadN \
${ncbi_f}${fna_f} "GRCz11" --star 
