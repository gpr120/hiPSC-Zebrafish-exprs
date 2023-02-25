work_f="/home/rstudio/RNAseq/"
threadN=4
species="Zebrafish"
ncbi_f=${work_f}"database/"${species}"/"
cd $ncbi_f
project_f=${work_f}"221211_Rit_zebra/"
read_pair="pair"


# クオリティチェック
cd ${project_f}"fastq_data"

if [ ${read_pair} = "single" ];then
    files="*.fastq.gz"
    for file in $files
    do
    fastqc -t $threadN --nogroup -o "../fastqc" -f fastq $file
    done
fi
if [ ${read_pair} = "pair" ];then
    files="RA*/*.fastqsanger.gz"
    for file1 in $files
    do
    save=${file1%*/*}
    fastqc -t $threadN --nogroup -o $save -f fastq $file1
    done
fi


# trimmomatic低品質領域を除去する
cd $work_f


SaveFolder=${project_f}"trimmomatic_01/" # 保存先フォルダ
mkdir $SaveFolder


if [ ${read_pair} = "single" ];then
    files=${project_f}"fastq_data/*.fastq.gz" # 解析するファイル
    for file in $files 
    do
        java -jar Trimmomatic-0.39/trimmomatic-0.39.jar \
            SE \
            -threads 10 \
            -phred33 \
            -summary $SaveFolder${file##*/}"_sumarry.txt" \
            $file \
            $SaveFolder${file##*/} \
            ILLUMINACLIP:Trimmomatic-0.39/adapters/TruSeq3-SE.fa:2:10:10 \
            MINLEN:30
    done
fi
if [ ${read_pair} = "pair" ];then
    files=${project_f}"fastq_data/*/forward.fastqsanger.gz" # 解析するファイル
    for file in $files 
    do
        id=${file%*/forward.fastqsanger.gz}
        echo $file
        TrimmomaticPE \
            -threads $threadN \
            -phred33 \
            -summary $SaveFolder${id##*/}"_sumarry.txt" \
            $file ${id}"/reverse.fastqsanger.gz" \
            $SaveFolder${id##*/}"_pair_1.fastq.gz" \
            $SaveFolder${id##*/}"_unpair_1.fastq.gz" \
            $SaveFolder${id##*/}"_pair_2.fastq.gz" \
            $SaveFolder${id##*/}"_unpair_2.fastq.gz" \
            ILLUMINACLIP:TruSeq3-PE-2.fa:2:30:10 \
            LEADING:15 \
            TRAILING:15 \
            SLIDINGWINDOW:4:15 \
            MINLEN:30
    done
fi

# trimmoaticのあとのクオリティチェック
cd $project_f

if [ ${read_pair} = "pair" ];then
    files="trimmomatic_01/*_pair_[1-2].fastq.gz"
    for file1 in $files
    do
    save=${file1%*/*}
    fastqc -t $threadN --nogroup -o $save -f fastq $file1
    done
fi



# mapping
cd $project_f
star_f=${work_f}"star_index/"${species}"/"

cd ${project_f}"STAR_Result"

if [ ${read_pair} = "single" ];then
    files=${project_f}"trimmomatic_01/*.fastq.gz"
    for file in $files
    do
        STAR --runThreadN 18 --genomeDir ${star_f}"GRCz11" \
        --outFileNamePrefix ${file%.fastq.gz} --quantMode TranscriptomeSAM \
        --outSAMtype BAM SortedByCoordinate \
        --readFilesIn $file --readFilesCommand zcat -c
    done
fi
if [ ${read_pair} = "pair" ];then
    files=${project_f}"trimmomatic_01/*_pair_1.fastq.gz"
    for file in $files
    do
        STAR --runThreadN 18 --genomeDir ${star_f}"GRCz11" \
        --outFileNamePrefix ${file%_pair_1.fastq.gz}
        --quantMode TranscriptomeSAM \
        --outSAMtype BAM SortedByCoordinate \
        --readFilesIn $file ${file%1.fastq.gz}"2.fastq.gz" --readFilesCommand zcat -c
    done
fi


# RSEM

# cd $work_f
# rsem-prepare-reference --gtf ${ncbi_f}"GCF_000001405.40_GRCh38.p14_genomic.gtf" -p 18 ${ncbi_f}"GCF_000001405.40_GRCh38.p14_genomic.fna" "rsem_index/h38_ncbi_RSEN_index" --star 



rsem_f=${work_f}"rsem_index/"${species}"/"

cd $project_f
files=${project_f}"trimmomatic_01/*.toTranscriptome.out.bam"
save_f=${project_f}"rsem_results_221216/"
mkdir $save_f


for file in $files
do
out_file=`echo ${file} | sed -r 's/.*\/(.*)Aligned.toTranscriptome.out.bam$/\1/'`
rsem-calculate-expression --alignments -p $threadN --paired-end  \
    --strandedness none \
    $file \
    ${rsem_f}"GRCz11" \
    ${save_f}${out_file}
done


# HDDに余裕が少ない時
files="sortedByCoord.out.bam"
rm $files