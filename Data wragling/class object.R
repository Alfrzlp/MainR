# class = cetakan kue
# object = kue

# ada s3, s4 dan ref class

karyawan <- setRefClass("karyawan",
  fields = list(nama = "character", gaji = "numeric", jabatan = "character"),
  methods = list(
    tambahgaji = function(p) {
      gaji <<- gaji + p
    },

    potonggaji = function(p) {
      gaji <<- gaji - p
    }
  )
)

ridson <- karyawan(nama = "ridson", gaji = 10, jabatan = "Data Scientis")

ridson$gaji

ridson$tambahgaji(10)
ridson$gaji


ridson$potonggaji(5)
ridson$gaji
