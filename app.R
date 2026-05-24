library(shiny)
library(shinydashboard)
library(DT)
library(png)
library(grDevices)

APP_DATA_DIR <- Sys.getenv("APP_DATA_DIR", unset = "")

data_path <- function(filename) {
  if (nzchar(APP_DATA_DIR)) file.path(APP_DATA_DIR, filename) else filename
}

ensure_parent_dir <- function(path) {
  parent <- dirname(path)
  if (nzchar(parent) && !dir.exists(parent)) dir.create(parent, recursive = TRUE)
}

STUDENTS_DB <- data_path("students_db.csv")
USERS_DB <- data_path("users_db.csv")
ANNOUNCEMENTS_DB <- data_path("announcements_db.csv")
DAILY_ATTENDANCE_DB <- data_path("daily_attendance_db.csv")
TIMETABLE_DB <- data_path("timetable_db.csv")
PHOTO_DIR <- "www/photos"
LOGO_FILES <- c(
  "www/college_logo.png",
  "www/college_logo.jpg",
  "www/college_logo.jpeg",
  "www/logo.png",
  "www/logo.jpg",
  "www/logo.jpeg"
)

DEPARTMENTS <- c("BCA", "B.COM", "BBA", "BA", "MCA", "MBA")
YEARS <- c("I Year", "II Year", "III Year", "IV Year")
LANG_OPTIONS <- c("Additional English", "Hindi", "Kannada")
FEE_OPTIONS <- c("Paid", "Pending", "Scholarship")
SEMESTER_OPTIONS <- paste("Semester", 1:6)
MAX_SUBJECT_SLOTS <- 9
STAFF_ROLE_OPTIONS <- c("Faculty", "HoD", "Principal")
TIMETABLE_DAY_OPTIONS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

default_subject_info <- data.frame(
  code = paste0("M", 1:8),
  paper_code = paste0("SUB", sprintf("%03d", 1:8)),
  subject = c(
    "Generic English",
    "Language 1",
    "R Programming",
    "Operating System Concepts",
    "Data Structure using C",
    "Constitutional Moral Values",
    "R Programming Lab",
    "Data Structure Lab"
  ),
  max_mark = c(100, 100, 100, 100, 100, 50, 50, 50),
  credits = c(4, 4, 4, 4, 4, 2, 3, 3),
  pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20),
  scheme = rep("Standard", 8),
  stringsAsFactors = FALSE
)

BCA_BNU_CURRICULUM <- list(
  "Semester 1" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BCA101", "BCA102", "BCA103", "BCA104", "BCA105", "BCA106P", "BCA107P", "BCA108P"),
    subject = c(
      "Language-I",
      "English-I",
      "Fundamentals of Computers",
      "Programming in C",
      "Computational Discrete Mathematics",
      "Office Automation Lab",
      "C Programming Lab",
      "Environmental Studies"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50),
    credits = c(3, 3, 4, 4, 4, 2, 2, 2),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20),
    scheme = rep("SEP", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 2" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BCA201", "BCA202", "BCA203", "BCA204", "BCA205", "BCA206P", "BCA207P", "BCA208P"),
    subject = c(
      "Language-II",
      "English-II",
      "Data Structures using C",
      "Statistical Methods using R",
      "Operating System Concepts",
      "Data Structures Lab",
      "R Programming Lab",
      "Constitutional Moral Values-I"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50),
    credits = c(3, 3, 4, 4, 4, 2, 2, 2),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20),
    scheme = rep("SEP", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 3" = data.frame(
    code = paste0("M", 1:9),
    paper_code = c("BCA301", "BCA302", "BCA303", "BCA304", "BCA305", "BCA306P", "BCA307P", "BCA308P", "BCA309D"),
    subject = c(
      "Language-III",
      "English-III",
      "Object Oriented Concepts using JAVA",
      "Database Management Systems",
      "Design and Analysis of Algorithms",
      "Java Programming Lab",
      "DBMS Lab",
      "Shell Programming Lab (SEC)",
      "Internet of Things / Cloud Computing (DSE)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50, 50),
    credits = c(3, 3, 4, 4, 4, 2, 2, 2, 2),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20, 20),
    scheme = rep("SEP", 9),
    stringsAsFactors = FALSE
  ),
  "Semester 4" = data.frame(
    code = paste0("M", 1:9),
    paper_code = c("BCA401", "BCA402", "BCA403", "BCA404", "BCA405", "BCA406P", "BCA407P", "BCA408P", "BCA409D"),
    subject = c(
      "Language-IV",
      "English-IV",
      "Python Programming",
      "Artificial Intelligence & Applications",
      "Computer Networks",
      "Python Programming Lab",
      "Artificial Intelligence Lab using Python",
      "Constitutional Moral Values-II",
      "Fundamentals of Data Science / Machine Learning (DSE)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50, 50),
    credits = c(3, 3, 4, 4, 4, 2, 2, 2, 2),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20, 20),
    scheme = rep("SEP", 9),
    stringsAsFactors = FALSE
  ),
  "Semester 5" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BCA501", "BCA502P", "BCA503", "BCA504P", "BCA505", "BCA506E", "BCA507V", "BCA508S"),
    subject = c(
      "Design & Analysis of Algorithms",
      "Design & Analysis of Algorithms Lab",
      "Statistical Computing and R Programming",
      "R Programming Lab",
      "Software Engineering",
      "Elective: Cloud Computing / Business Intelligence",
      "Digital Marketing (Vocational)",
      "Cyber Security (SEC)"
    ),
    max_mark = c(100, 50, 100, 50, 100, 100, 100, 50),
    credits = c(4, 2, 4, 2, 4, 4, 2, 2),
    pass_mark = c(40, 20, 40, 20, 40, 40, 40, 20),
    scheme = rep("NEP", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 6" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("BCA601", "BCA602", "BCA603P", "BCA604P", "BCA605E", "BCA606V", "BCA607S"),
    subject = c(
      "Artificial Intelligence and Applications",
      "PHP and MySQL",
      "PHP and MySQL Lab",
      "Project Work",
      "Elective: Fundamentals of Data Science / Mobile Application Development",
      "Web Content Management System (Vocational)",
      "Logical Reasoning (SEC)"
    ),
    max_mark = c(100, 100, 50, 100, 100, 100, 50),
    credits = c(4, 4, 2, 6, 4, 2, 2),
    pass_mark = c(40, 40, 20, 40, 40, 40, 20),
    scheme = rep("NEP", 7),
    stringsAsFactors = FALSE
  )
)

MCA_BNU_CURRICULUM <- list(
  "Semester 1" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("MCA101T", "MCA102T", "MCA103T", "MCA104T", "MCA105T", "MCA106T", "MCA107P", "MCA108P"),
    subject = c(
      "Object Oriented Programming with Java",
      "Advanced Software Engineering",
      "Mathematical Foundations",
      "Advanced Database Management System",
      "Data Structures and Algorithms",
      "Theory of Computation",
      "Java Programming Lab",
      "Data Structures and Algorithm Lab"
    ),
    max_mark = rep(100, 8),
    credits = c(4, 4, 4, 4, 4, 4, 2, 2),
    pass_mark = rep(40, 8),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 2" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("MCA201T", "MCA202T", "MCA203T", "MCA204T", "MCA205T", "MCA206T", "MCA207P", "MCA208P"),
    subject = c(
      "Artificial Intelligence",
      "Web Technologies",
      "Advanced Python Programming",
      "Operating System and Linux",
      "Network and Information Security",
      "Cloud Computing",
      "Web Technologies Lab",
      "Python Programming Lab"
    ),
    max_mark = rep(100, 8),
    credits = c(4, 4, 4, 4, 4, 4, 2, 2),
    pass_mark = rep(40, 8),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 3" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("MCA301T", "MCA302T", "MCA303T", "MCA304T", "MCA305T", "MCA306P", "MCA307P"),
    subject = c(
      "Research Methodology and IPR",
      "Data Science",
      "Open Elective",
      "Elective-I",
      "Elective-II",
      "Data Science Lab",
      "Mini Project"
    ),
    max_mark = rep(100, 7),
    credits = c(4, 4, 4, 4, 4, 4, 4),
    pass_mark = rep(40, 7),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  ),
  "Semester 4" = data.frame(
    code = "M1",
    paper_code = "MCA401P",
    subject = "Main Project",
    max_mark = 400,
    credits = 16,
    pass_mark = 160,
    scheme = "BNU",
    stringsAsFactors = FALSE
  )
)

BCOM_BNU_CURRICULUM <- list(
  "Semester 1" = data.frame(
    code = paste0("M", 1:9),
    paper_code = c("BCOM101", "BCOM102", "BCOM103", "BCOM104", "BCOM105", "BCOM106", "BCOM107", "BCOM108", "BCOM109"),
    subject = c(
      "Language-I",
      "Language-II",
      "Financial Accounting",
      "Management Principles & Applications",
      "Principles of Marketing",
      "Digital Fluency",
      "Physical Education (Yoga)",
      "Health & Wellness",
      "OEC: Accounting for Everyone / Financial Literacy / Entrepreneurship"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50, 100),
    credits = c(4, 4, 4, 4, 4, 1, 1, 1, 2),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20, 40),
    scheme = rep("BNU", 9),
    stringsAsFactors = FALSE
  ),
  "Semester 2" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BCOM201", "BCOM202", "BCOM203", "BCOM204", "BCOM205", "BCOM206", "BCOM207", "BCOM208"),
    subject = c(
      "Language-I",
      "Language-II",
      "Advanced Financial Accounting",
      "Corporate Administration / Business Mathematics",
      "Law & Practice of Banking",
      "Environmental Studies",
      "Sports / NCC / NSS",
      "OEC: Financial Environment / Stock Market / Event Management"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 100, 100),
    credits = c(4, 4, 4, 4, 4, 1, 2, 2),
    pass_mark = c(40, 40, 40, 40, 40, 20, 40, 40),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 3" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BCOM301", "BCOM302", "BCOM303", "BCOM304", "BCOM305", "BCOM306", "BCOM307", "BCOM308"),
    subject = c(
      "Language-I",
      "Language-II",
      "Corporate Accounting",
      "Marketing Management",
      "Business Law-II",
      "Financial Education & Investment Awareness",
      "Sports / NCC",
      "OEC: Social Media Marketing / Rural Marketing"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 100),
    credits = c(4, 4, 4, 4, 4, 1, 1, 3),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 40),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 4" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BCOM401", "BCOM402", "BCOM403", "BCOM404", "BCOM405", "BCOM406", "BCOM407", "BCOM408"),
    subject = c(
      "Language-I",
      "Language-II",
      "Management Accounting",
      "Business Analytics / Financial Markets & Services",
      "Financial Management",
      "India & Indian Constitution",
      "Sports / NCC",
      "Business Leadership Skills / Personal Wealth Management"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50),
    credits = c(4, 4, 4, 4, 4, 1, 1, 3),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 5" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("BCOM501", "BCOM502", "BCOM503", "BCOM504", "BCOM505", "BCOM506", "BCOM507"),
    subject = c(
      "Goods & Services Tax",
      "Income Tax",
      "Costing Methods & Techniques",
      "Auditing",
      "Specialization-I",
      "Specialization-II",
      "Business Research Methodology"
    ),
    max_mark = rep(100, 7),
    credits = c(4, 4, 4, 4, 3, 3, 3),
    pass_mark = rep(40, 7),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  ),
  "Semester 6" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("BCOM601", "BCOM602", "BCOM603", "BCOM604", "BCOM605", "BCOM606", "BCOM607"),
    subject = c(
      "Business Taxation",
      "Income Tax-II",
      "Management Accounting",
      "Mercantile Law",
      "Specialization-I",
      "Specialization-II",
      "Project Work"
    ),
    max_mark = rep(100, 7),
    credits = c(4, 4, 4, 4, 3, 3, 3),
    pass_mark = rep(40, 7),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  )
)

BBA_BNU_CURRICULUM <- list(
  "Semester 1" = data.frame(
    code = paste0("M", 1:9),
    paper_code = c("BBA101", "BBA102", "BBA103", "BBA104", "BBA105", "BBA106", "BBA107", "BBA108", "BBA109"),
    subject = c(
      "Language-I",
      "Language-II",
      "Management Principles and Applications",
      "Fundamentals of Business Accounting",
      "Accounting Management",
      "Digital Fluency",
      "Physical Education - Yoga",
      "Health & Wellness",
      "Office Management / Business Organisation (OEC)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50, 100),
    credits = c(4, 4, 4, 4, 4, 1, 1, 1, 2),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20, 40),
    scheme = rep("BNU", 9),
    stringsAsFactors = FALSE
  ),
  "Semester 2" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BBA201", "BBA202", "BBA203", "BBA204", "BBA205", "BBA206", "BBA207", "BBA208"),
    subject = c(
      "Language-I",
      "Language-II",
      "Financial Accounting and Reporting",
      "Human Resource Management",
      "Business Mathematics",
      "Environmental Studies",
      "Sports / NCC / NSS",
      "People Management / Retail Management (OEC)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 100),
    credits = c(4, 4, 4, 4, 4, 1, 1, 3),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 40),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 3" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BBA301", "BBA302", "BBA303", "BBA304", "BBA305", "BBA306", "BBA307", "BBA308"),
    subject = c(
      "Language-I",
      "Language-II",
      "Cost Accounting",
      "Organizational Behaviour",
      "Statistics for Business",
      "Financial Education & Investment Awareness",
      "Sports / NCC",
      "Social Media Marketing / Rural Marketing (OEC)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 100),
    credits = c(4, 4, 4, 4, 4, 1, 1, 3),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 40),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 4" = data.frame(
    code = paste0("M", 1:8),
    paper_code = c("BBA401", "BBA402", "BBA403", "BBA404", "BBA405", "BBA406", "BBA407", "BBA408"),
    subject = c(
      "Language-I",
      "Language-II",
      "Management Accounting",
      "Business Analytics / Financial Markets & Services",
      "Financial Management",
      "India & Indian Constitution",
      "Sports / NCC",
      "Business Leadership Skills / Personal Wealth Management (OEC)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 50, 50, 50),
    credits = c(4, 4, 4, 4, 4, 1, 1, 3),
    pass_mark = c(40, 40, 40, 40, 40, 20, 20, 20),
    scheme = rep("BNU", 8),
    stringsAsFactors = FALSE
  ),
  "Semester 5" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("BBA501", "BBA502", "BBA503", "BBA504", "BBA505", "BBA506", "BBA507"),
    subject = c(
      "Production and Operations Management",
      "Income Tax",
      "Banking Law and Practice",
      "Elective-I (MIS / HRM / IDA / IR)",
      "Elective-II (MIS / HRM / IDA / IR)",
      "Vocational: Insurance Technology / Digital Marketing",
      "Employability Skills / Cyber Security"
    ),
    max_mark = c(100, 100, 100, 100, 100, 100, 50),
    credits = c(4, 4, 4, 4, 4, 2, 2),
    pass_mark = c(40, 40, 40, 40, 40, 40, 20),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  ),
  "Semester 6" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("BBA601", "BBA602", "BBA603", "BBA604", "BBA605", "BBA606", "BBA607"),
    subject = c(
      "Business Law",
      "Income Tax-II",
      "International Business",
      "Elective-III (MIS / HRM / IDA / IR)",
      "Elective-IV (MIS / HRM / IDA / IR)",
      "Vocational: Goods & Services Tax / ERP Applications",
      "Internship"
    ),
    max_mark = rep(100, 7),
    credits = c(4, 4, 4, 4, 4, 2, 2),
    pass_mark = rep(40, 7),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  )
)

MBA_BNU_CURRICULUM <- list(
  "Semester 1" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("MBA1.1", "MBA1.2", "MBA1.3", "MBA1.4", "MBA1.5", "MBA1.6", "MBA1.7"),
    subject = c(
      "Management & Organizational Behaviour",
      "Accounting for Managers",
      "Statistical Analysis for Managerial Decision",
      "Marketing Management",
      "Economics for Managers",
      "Innovation & Entrepreneurship",
      "Corporate Communication (SEC)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 100, 100),
    credits = c(4, 4, 4, 4, 4, 4, 2),
    pass_mark = c(40, 40, 40, 40, 40, 40, 40),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  ),
  "Semester 2" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("MBA2.1", "MBA2.2", "MBA2.3", "MBA2.4", "MBA2.5", "MBA2.6", "MBA2.7"),
    subject = c(
      "IT and Analytics for Business Leaders",
      "Quantitative Techniques and Operations Management",
      "Human Capital Management",
      "Financial Management",
      "International Business Dynamics",
      "Family Business Management and Legal Framework",
      "Strategic Soft Skills for Managers (SEC)"
    ),
    max_mark = c(100, 100, 100, 100, 100, 100, 100),
    credits = c(4, 4, 4, 4, 4, 4, 2),
    pass_mark = c(40, 40, 40, 40, 40, 40, 40),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  ),
  "Semester 3" = data.frame(
    code = paste0("M", 1:7),
    paper_code = c("MBA3.1", "MBA3.2", "MBA3.3", "MBA3.4", "MBA3.5", "MBA3.6", "MBA3.7"),
    subject = c(
      "Business Research Methods",
      "Strategic Management & Corporate Governance",
      "Elective Course 1",
      "Elective Course 2",
      "Elective Course 3",
      "Elective Course 4",
      "Certification Course / Internship"
    ),
    max_mark = c(100, 100, 100, 100, 100, 100, 50),
    credits = c(4, 4, 4, 4, 4, 4, 2),
    pass_mark = c(40, 40, 40, 40, 40, 40, 20),
    scheme = rep("BNU", 7),
    stringsAsFactors = FALSE
  ),
  "Semester 4" = data.frame(
    code = paste0("M", 1:5),
    paper_code = c("MBA4.1", "MBA4.2", "MBA4.3", "MBA4.4", "MBA4.5"),
    subject = c(
      "Elective Course 1",
      "Elective Course 2",
      "Elective Course 3",
      "Elective Course 4",
      "Dissertation Project"
    ),
    max_mark = c(100, 100, 100, 100, 250),
    credits = c(4, 4, 4, 4, 10),
    pass_mark = c(40, 40, 40, 40, 100),
    scheme = rep("BNU", 5),
    stringsAsFactors = FALSE
  )
)

empty_students <- function() {
  empty_numeric_cols <- function(prefix) {
    stats::setNames(replicate(MAX_SUBJECT_SLOTS, numeric(), simplify = FALSE), paste0(prefix, seq_len(MAX_SUBJECT_SLOTS)))
  }
  mark_cols <- empty_numeric_cols("M")
  internal1_cols <- empty_numeric_cols("IE1_M")
  internal2_cols <- empty_numeric_cols("IE2_M")
  assignment_cols <- empty_numeric_cols("ASG_M")
  attendance_taken_cols <- stats::setNames(replicate(MAX_SUBJECT_SLOTS, numeric(), simplify = FALSE), paste0("A", seq_len(MAX_SUBJECT_SLOTS), "_Taken"))
  attendance_attended_cols <- stats::setNames(replicate(MAX_SUBJECT_SLOTS, numeric(), simplify = FALSE), paste0("A", seq_len(MAX_SUBJECT_SLOTS), "_Attended"))

  as.data.frame(
    c(
      list(
        RegNo = character(),
        Name = character(),
        Dept = character(),
        Year = character(),
        Semester = character(),
        Scheme = character(),
        Photo = character(),
        Lang1 = character()
      ),
      mark_cols,
      internal1_cols,
      internal2_cols,
      assignment_cols,
      attendance_taken_cols,
      attendance_attended_cols,
      list(
        Total = numeric(),
        TotalCredits = numeric(),
        Percentage = numeric(),
        SGPA = numeric(),
        PrevCGPA = numeric(),
        CGPA = numeric(),
        Grade = character(),
        Attendance = numeric(),
        FeeStatus = character(),
        FeeTotalAmount = numeric(),
        FeePaidAmount = numeric(),
        FeeScholarshipAmount = numeric(),
        FeeBalanceAmount = numeric(),
        FeeLastPaymentDate = character(),
        FeeReceiptNo = character(),
        FeeRemarks = character(),
        Mentor = character(),
        AddressLine1 = character(),
        AddressLine2 = character(),
        City = character(),
        State = character(),
        Pincode = character(),
        UpdatedAt = character()
      )
    ),
    stringsAsFactors = FALSE
  )
}

empty_users <- function() {
  data.frame(
    RegNo = character(),
    Name = character(),
    Email = character(),
    Password = character(),
    Role = character(),
    StaffRole = character(),
    Approved = character(),
    Photo = character(),
    Dept = character(),
    Year = character(),
    CRAccess = character(),
    CRDept = character(),
    CRYear = character(),
    CRSemester = character(),
    CRAssignedBy = character(),
    CRAssignedByRole = character(),
    CRAssignedOn = character(),
    AddressLine1 = character(),
    AddressLine2 = character(),
    City = character(),
    State = character(),
    Pincode = character(),
    UpdatedAt = character(),
    stringsAsFactors = FALSE
  )
}

empty_announcements <- function() {
  data.frame(
    Title = character(),
    Body = character(),
    Audience = character(),
    PostedOn = character(),
    stringsAsFactors = FALSE
  )
}

empty_daily_attendance <- function() {
  data.frame(
    Date = character(),
    SubjectCode = character(),
    SubjectName = character(),
    ClassTime = character(),
    MarkedByRegNo = character(),
    MarkedByName = character(),
    MarkedByRole = character(),
    FacultyRegNo = character(),
    FacultyName = character(),
    StudentRegNo = character(),
    StudentName = character(),
    Dept = character(),
    Year = character(),
    Semester = character(),
    Status = character(),
    RecordedAt = character(),
    stringsAsFactors = FALSE
  )
}

empty_timetable <- function() {
  data.frame(
    Dept = character(),
    Semester = character(),
    SlotOrder = numeric(),
    Day = character(),
    Time = character(),
    Subject = character(),
    TeacherName = character(),
    UpdatedByRegNo = character(),
    UpdatedByRole = character(),
    UpdatedAt = character(),
    stringsAsFactors = FALSE
  )
}

read_safe_csv <- function(path, empty_df) {
  if (!file.exists(path)) {
    ensure_parent_dir(path)
    write.csv(empty_df, path, row.names = FALSE)
    return(empty_df)
  }

  tryCatch(
    read.csv(path, stringsAsFactors = FALSE),
    error = function(e) empty_df
  )
}

ensure_schema <- function(df, template) {
  for (col_name in names(template)) {
    if (!col_name %in% names(df)) {
      if (nrow(df) == 0) {
        df[[col_name]] <- template[[col_name]]
      } else if (is.numeric(template[[col_name]])) {
        df[[col_name]] <- rep(0, nrow(df))
      } else {
        df[[col_name]] <- rep("", nrow(df))
      }
    }
  }

  df <- df[, names(template), drop = FALSE]

  numeric_cols <- names(template)[vapply(template, is.numeric, logical(1))]
  char_cols <- setdiff(names(template), numeric_cols)

  for (col_name in numeric_cols) {
    df[[col_name]] <- suppressWarnings(as.numeric(df[[col_name]]))
    df[[col_name]][is.na(df[[col_name]])] <- 0
  }

  for (col_name in char_cols) {
    df[[col_name]] <- as.character(df[[col_name]])
    df[[col_name]][is.na(df[[col_name]])] <- ""
  }

  df
}

safe_bootstrap_write_csv <- function(data, path) {
  ensure_parent_dir(path)
  last_error <- "Unknown write error."

  for (attempt in seq_len(6)) {
    temp_path <- tempfile(
      pattern = paste0(tools::file_path_sans_ext(basename(path)), "_"),
      tmpdir = dirname(path),
      fileext = ".csv"
    )

    success <- tryCatch({
      write.csv(data, temp_path, row.names = FALSE)
      copied <- suppressWarnings(file.copy(temp_path, path, overwrite = TRUE, copy.mode = TRUE))
      if (!isTRUE(copied)) stop("The destination file is locked or temporarily unavailable.")
      TRUE
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      FALSE
    })

    if (file.exists(temp_path)) unlink(temp_path, force = TRUE)
    if (success) return(invisible(TRUE))
    Sys.sleep(0.2 * attempt)
  }

  stop(
    paste0(
      "Unable to prepare ", basename(path),
      ". Close any open CSV or Excel window using this file and restart the app. ",
      last_error
    ),
    call. = FALSE
  )
}

normalize_user_records <- function(users) {
  if (nrow(users) == 0) return(users)

  users$RegNo <- toupper(trimws(users$RegNo))
  users$Role <- tolower(trimws(users$Role))
  users$Name <- ifelse(nzchar(trimws(users$Name)), trimws(users$Name), users$RegNo)
  users$StaffRole <- ifelse(users$Role == "faculty" & nzchar(trimws(users$StaffRole)), trimws(users$StaffRole), users$StaffRole)
  users$StaffRole <- ifelse(users$Role == "faculty" & !nzchar(users$StaffRole), "Faculty", users$StaffRole)
  users$Approved <- ifelse(users$Role == "faculty" & nzchar(trimws(users$Approved)), trimws(users$Approved), users$Approved)
  users$Approved <- ifelse(users$Role == "faculty" & !nzchar(users$Approved), "Pending", users$Approved)
  users$Approved <- ifelse(users$Role != "faculty" & !nzchar(users$Approved), "Approved", users$Approved)
  users$CRAccess <- ifelse(users$Role == "student" & tolower(trimws(users$CRAccess)) %in% c("yes", "y", "assigned", "true"), "Yes", "No")
  users$CRDept <- ifelse(users$Role == "student", trimws(users$CRDept), "")
  users$CRYear <- ifelse(users$Role == "student", trimws(users$CRYear), "")
  users$CRSemester <- ifelse(users$Role == "student", trimws(users$CRSemester), "")
  users$CRAssignedBy <- ifelse(users$Role == "student", trimws(users$CRAssignedBy), "")
  users$CRAssignedByRole <- ifelse(users$Role == "student", trimws(users$CRAssignedByRole), "")
  users$CRAssignedOn <- ifelse(users$Role == "student", trimws(users$CRAssignedOn), "")
  users
}

students_template <- empty_students()
users_template <- empty_users()
announcements_template <- empty_announcements()
daily_attendance_template <- empty_daily_attendance()
timetable_template <- empty_timetable()

if (!dir.exists("www")) dir.create("www")
if (!dir.exists(PHOTO_DIR)) dir.create(PHOTO_DIR, recursive = TRUE)

safe_bootstrap_write_csv(
  ensure_schema(read_safe_csv(STUDENTS_DB, students_template), students_template),
  STUDENTS_DB
)
safe_bootstrap_write_csv(
  normalize_user_records(ensure_schema(read_safe_csv(USERS_DB, users_template), users_template)),
  USERS_DB
)
safe_bootstrap_write_csv(
  ensure_schema(read_safe_csv(ANNOUNCEMENTS_DB, announcements_template), announcements_template),
  ANNOUNCEMENTS_DB
)
safe_bootstrap_write_csv(
  ensure_schema(read_safe_csv(DAILY_ATTENDANCE_DB, daily_attendance_template), daily_attendance_template),
  DAILY_ATTENDANCE_DB
)
safe_bootstrap_write_csv(
  ensure_schema(read_safe_csv(TIMETABLE_DB, timetable_template), timetable_template),
  TIMETABLE_DB
)

pass_threshold <- function(max_mark) {
  if (max_mark <= 0) return(0)
  ceiling(max_mark * 0.35)
}

get_pass_marks <- function(subject_info) {
  vapply(subject_info$max_mark, pass_threshold, numeric(1))
}

latest_student_record <- function(db, reg_no) {
  rows <- db[db$RegNo == reg_no, , drop = FALSE]
  if (nrow(rows) == 0) return(rows)
  updated_at <- suppressWarnings(as.POSIXct(rows$UpdatedAt, tz = "Asia/Calcutta"))
  updated_at[is.na(updated_at)] <- as.POSIXct("1970-01-01", tz = "Asia/Calcutta")
  rows[order(updated_at, decreasing = TRUE), , drop = FALSE][1, , drop = FALSE]
}

student_semester_records <- function(db, reg_no) {
  rows <- db[db$RegNo == reg_no, , drop = FALSE]
  if (nrow(rows) == 0) return(rows)
  updated_at <- suppressWarnings(as.POSIXct(rows$UpdatedAt, tz = "Asia/Calcutta"))
  updated_at[is.na(updated_at)] <- as.POSIXct("1970-01-01", tz = "Asia/Calcutta")
  rows[order(updated_at, decreasing = TRUE), , drop = FALSE]
}

get_subject_info <- function(dept, semester) {
  if (identical(dept, "BCA") && semester %in% names(BCA_BNU_CURRICULUM)) {
    return(BCA_BNU_CURRICULUM[[semester]])
  }
  if (identical(dept, "MCA") && semester %in% names(MCA_BNU_CURRICULUM)) {
    return(MCA_BNU_CURRICULUM[[semester]])
  }
  if (identical(dept, "B.COM") && semester %in% names(BCOM_BNU_CURRICULUM)) {
    return(BCOM_BNU_CURRICULUM[[semester]])
  }
  if (identical(dept, "BBA") && semester %in% names(BBA_BNU_CURRICULUM)) {
    return(BBA_BNU_CURRICULUM[[semester]])
  }
  if (identical(dept, "MBA") && semester %in% names(MBA_BNU_CURRICULUM)) {
    return(MBA_BNU_CURRICULUM[[semester]])
  }
  default_subject_info
}

get_semester_choices <- function(dept) {
  if (identical(dept, "BCA")) return(names(BCA_BNU_CURRICULUM))
  if (identical(dept, "MCA")) return(names(MCA_BNU_CURRICULUM))
  if (identical(dept, "B.COM")) return(names(BCOM_BNU_CURRICULUM))
  if (identical(dept, "BBA")) return(names(BBA_BNU_CURRICULUM))
  if (identical(dept, "MBA")) return(names(MBA_BNU_CURRICULUM))
  SEMESTER_OPTIONS
}

get_total_max <- function(subject_info) {
  sum(subject_info$max_mark)
}

get_total_credits <- function(subject_info) {
  sum(subject_info$credits)
}

is_admin_user <- function(role) {
  identical(role, "admin")
}

is_staff_role <- function(role) {
  role %in% c("admin", "faculty")
}

is_faculty_user <- function(role, staff_role = "") {
  identical(role, "faculty") && identical(tolower(staff_role), "faculty")
}

is_hod_user <- function(role, staff_role = "") {
  identical(role, "faculty") && identical(tolower(staff_role), "hod")
}

is_principal_user <- function(role, staff_role = "") {
  identical(role, "faculty") && identical(tolower(staff_role), "principal")
}

has_cr_attendance_access <- function(role, account = NULL) {
  identical(role, "student") &&
    !is.null(account) &&
    nrow(account) > 0 &&
    identical(tolower(trimws(account$CRAccess[1])), "yes")
}

can_assign_cr_access <- function(role, staff_role = "") {
  identical(role, "faculty")
}

can_manage_daily_attendance <- function(role, staff_role = "", account = NULL) {
  identical(role, "faculty") || has_cr_attendance_access(role, account)
}

can_manage_timetable <- function(role, staff_role = "") {
  is_admin_user(role) || is_hod_user(role, staff_role) || is_principal_user(role, staff_role)
}

is_valid_uibs_regno <- function(reg_no) {
  grepl("^U19TO[A-Za-z0-9]+$", toupper(trimws(reg_no)))
}

is_valid_college_email <- function(email) {
  grepl("^[A-Za-z0-9._%+-]+@uibsblr\\.com$", trimws(email), ignore.case = TRUE)
}

full_address <- function(line1 = "", line2 = "", city = "", state = "", pincode = "") {
  parts <- c(line1, line2, city, state, pincode)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) "Address not updated" else paste(parts, collapse = ", ")
}

timetable_slot_count <- function(dept, semester, current_rows = NULL) {
  subject_count <- nrow(get_subject_info(dept, semester))
  existing_count <- if (is.null(current_rows)) 0 else nrow(current_rows)
  max(8, subject_count, existing_count)
}

college_highlights <- function() {
  image_path <- app_logo_src()
  data.frame(
    Title = c("Academic Excellence", "Campus Innovation", "Industry Connect"),
    Caption = c(
      "Semester toppers, merit culture and rigorous mentorship.",
      "Workshops, labs, projects and skill-building initiatives across departments.",
      "Guest lectures, internships and placement-oriented engagement."
    ),
    Image = rep(image_path, 3),
    stringsAsFactors = FALSE
  )
}

requires_language <- function(dept, semester) {
  if (dept %in% c("MBA", "MCA")) return(FALSE)
  if (dept %in% c("BCA", "B.COM", "BBA") && semester %in% c("Semester 5", "Semester 6")) return(FALSE)
  TRUE
}

is_results_published <- function(student_row) {
  if (nrow(student_row) == 0) return(FALSE)
  nzchar(student_row$Grade[1]) && !identical(student_row$Grade[1], "N/A")
}

attendance_taken_col <- function(idx) paste0("A", idx, "_Taken")
attendance_attended_col <- function(idx) paste0("A", idx, "_Attended")
internal1_col <- function(idx) paste0("IE1_M", idx)
internal2_col <- function(idx) paste0("IE2_M", idx)
assignment_col <- function(idx) paste0("ASG_M", idx)

round_whole <- function(x) {
  ifelse(is.na(x), 0, floor(as.numeric(x) + 0.5))
}

external_max_mark <- function(max_mark) {
  ifelse(max_mark >= 100, 80, ifelse(max_mark >= 50, 40, max_mark))
}

internal_max_mark <- function(max_mark) {
  pmax(max_mark - external_max_mark(max_mark), 0)
}

internal_exam_raw_max <- function(max_mark) {
  ifelse(max_mark >= 100, 30, ifelse(max_mark >= 50, 40, max_mark))
}

internal_component_caps <- function(max_mark) {
  internal_cap <- internal_max_mark(max_mark)
  exam_cap <- round_whole(internal_cap * 0.50)
  remaining_cap <- pmax(internal_cap - exam_cap, 0)
  assignment_cap <- ceiling(remaining_cap / 2)
  attendance_cap <- floor(remaining_cap / 2)
  data.frame(
    AssignmentCap = assignment_cap,
    AttendanceCap = attendance_cap,
    ExamCap = exam_cap,
    InternalCap = internal_cap,
    stringsAsFactors = FALSE
  )
}

attendance_percentage_from_vectors <- function(classes_taken, classes_attended) {
  total_taken <- sum(as.numeric(classes_taken), na.rm = TRUE)
  total_attended <- sum(as.numeric(classes_attended), na.rm = TRUE)
  if (!is.finite(total_taken) || total_taken <= 0) return(0)
  round_whole((total_attended / total_taken) * 100)
}

student_attendance_details <- function(student_row, subject_info = NULL) {
  if (is.null(subject_info)) subject_info <- get_subject_info(student_row$Dept[1], resolve_student_semester(student_row))
  n_subjects <- nrow(subject_info)
  if (n_subjects == 0) {
    return(data.frame(PaperCode = character(), Subject = character(), ClassesTaken = numeric(), ClassesAttended = numeric(), AttendancePct = numeric(), stringsAsFactors = FALSE))
  }

  taken_cols <- vapply(seq_len(n_subjects), attendance_taken_col, character(1))
  attended_cols <- vapply(seq_len(n_subjects), attendance_attended_col, character(1))
  classes_taken <- as.numeric(student_row[1, taken_cols, drop = TRUE])
  classes_attended <- as.numeric(student_row[1, attended_cols, drop = TRUE])
  classes_taken[is.na(classes_taken)] <- 0
  classes_attended[is.na(classes_attended)] <- 0
  classes_attended <- pmin(classes_attended, classes_taken)
  attendance_pct <- ifelse(classes_taken > 0, round_whole((classes_attended / classes_taken) * 100), 0)

  data.frame(
    PaperCode = subject_info$paper_code,
    Subject = subject_info$subject,
    ClassesTaken = classes_taken,
    ClassesAttended = classes_attended,
    AttendancePct = attendance_pct,
    stringsAsFactors = FALSE
  )
}

student_overall_attendance <- function(student_row, subject_info = NULL) {
  details <- student_attendance_details(student_row, subject_info)
  attendance_percentage_from_vectors(details$ClassesTaken, details$ClassesAttended)
}

is_attendance_eligible <- function(student_row, subject_info = NULL, threshold = 75) {
  student_overall_attendance(student_row, subject_info) >= threshold
}

attendance_bonus_from_pct <- function(attendance_pct, max_bonus) {
  if (!is.finite(attendance_pct) || max_bonus <= 0) return(0)
  round_whole(pmax(pmin(attendance_pct, 100), 0) / 100 * max_bonus)
}

student_internal_details <- function(student_row, subject_info = NULL) {
  if (is.null(subject_info)) subject_info <- get_subject_info(student_row$Dept[1], resolve_student_semester(student_row))
  n_subjects <- nrow(subject_info)
  if (n_subjects == 0) {
    return(data.frame(
      PaperCode = character(),
      Subject = character(),
      Internal1 = numeric(),
      Internal2 = numeric(),
      Assignment = numeric(),
      AttendanceBonus = numeric(),
      InternalScore = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  attendance_details <- student_attendance_details(student_row, subject_info)
  internal1 <- vapply(seq_len(n_subjects), function(i) as.numeric(student_row[[internal1_col(i)]][1]), numeric(1))
  internal2 <- vapply(seq_len(n_subjects), function(i) as.numeric(student_row[[internal2_col(i)]][1]), numeric(1))
  assignment_raw <- vapply(seq_len(n_subjects), function(i) as.numeric(student_row[[assignment_col(i)]][1]), numeric(1))
  internal1[is.na(internal1)] <- 0
  internal2[is.na(internal2)] <- 0
  assignment_raw[is.na(assignment_raw)] <- 0

  cap_info <- internal_component_caps(subject_info$max_mark)
  internal_cap <- cap_info$InternalCap
  assignment_cap <- cap_info$AssignmentCap
  attendance_cap <- cap_info$AttendanceCap
  exam_cap <- cap_info$ExamCap
  exam_raw_max <- internal_exam_raw_max(subject_info$max_mark)

  assignment_score <- round_whole(pmax(pmin(assignment_raw, assignment_cap), 0))
  attendance_bonus <- vapply(seq_len(n_subjects), function(i) attendance_bonus_from_pct(attendance_details$AttendancePct[i], attendance_cap[i]), numeric(1))
  exam_average <- (internal1 + internal2) / 2
  exam_score <- ifelse(exam_raw_max > 0, round_whole(pmax(pmin(exam_average, exam_raw_max), 0) / exam_raw_max * exam_cap), 0)
  internal_score <- round_whole(pmax(pmin(assignment_score + attendance_bonus + exam_score, internal_cap), 0))

  data.frame(
    PaperCode = subject_info$paper_code,
    Subject = subject_info$subject,
    Internal1 = internal1,
    Internal2 = internal2,
    Assignment = assignment_score,
    AttendanceBonus = attendance_bonus,
    InternalScore = internal_score,
    stringsAsFactors = FALSE
  )
}

student_total_marks <- function(student_row, subject_info = NULL) {
  if (is.null(subject_info)) subject_info <- get_subject_info(student_row$Dept[1], resolve_student_semester(student_row))
  external_marks <- sanitize_marks(student_row[1, subject_info$code, drop = TRUE], data.frame(max_mark = external_max_mark(subject_info$max_mark)))
  internal_details <- student_internal_details(student_row, subject_info)
  round_whole(pmax(pmin(external_marks + internal_details$InternalScore, subject_info$max_mark), 0))
}

with_attendance_metric <- function(db) {
  if (nrow(db) == 0) {
    db$AttendanceMetric <- numeric(0)
    return(db)
  }

  db$AttendanceMetric <- vapply(seq_len(nrow(db)), function(i) student_overall_attendance(db[i, , drop = FALSE]), numeric(1))
  db
}

resolve_student_semester <- function(student_row) {
  semester <- student_row$Semester[1]
  if (nzchar(semester)) return(semester)
  choices <- get_semester_choices(student_row$Dept[1])
  if (length(choices) > 0) choices[1] else "Semester 1"
}

resolve_student_scheme <- function(student_row, subject_info) {
  scheme <- student_row$Scheme[1]
  if (nzchar(scheme)) return(scheme)
  unique(subject_info$scheme)[1]
}

sanitize_marks <- function(marks, subject_info) {
  marks <- as.numeric(marks)
  marks[is.na(marks)] <- 0
  pmax(pmin(marks, subject_info$max_mark), 0)
}

student_snapshot <- function(student_row) {
  semester <- resolve_student_semester(student_row)
  subject_info <- get_subject_info(student_row$Dept[1], semester)
  marks <- student_total_marks(student_row, subject_info)
  results <- compute_results(marks, subject_info, student_row$PrevCGPA[1])
  list(
    semester = semester,
    scheme = resolve_student_scheme(student_row, subject_info),
    subject_info = subject_info,
    marks = marks,
    results = results
  )
}

format_percent <- function(x) {
  paste0(round_whole(x), "%")
}

calculate_fee_balance <- function(total_amount, paid_amount, scholarship_amount = 0) {
  total_amount <- ifelse(is.na(total_amount), 0, total_amount)
  paid_amount <- ifelse(is.na(paid_amount), 0, paid_amount)
  scholarship_amount <- ifelse(is.na(scholarship_amount), 0, scholarship_amount)
  round_whole(max(total_amount - paid_amount - scholarship_amount, 0))
}

grade_color <- function(grade) {
  switch(
    grade,
    "A+" = "#1f7a4d",
    "A" = "#0f766e",
    "B" = "#d97706",
    "C" = "#b45309",
    "F" = "#b91c1c",
    "#64748b"
  )
}

safe_photo_path <- function(photo) {
  path <- file.path("www", photo)
  if (nzchar(photo) && file.exists(path)) photo else ""
}

app_logo_file <- function() {
  existing <- LOGO_FILES[file.exists(LOGO_FILES)]
  if (length(existing)) existing[1] else ""
}

app_logo_src <- function() {
  logo_file <- app_logo_file()
  if (nzchar(logo_file)) basename(logo_file) else ""
}

brand_title_ui <- function() {
  logo_src <- app_logo_src()
  tags$div(
    class = "brand-lockup",
    if (nzchar(logo_src)) {
      tags$img(src = logo_src, class = "brand-lockup-img")
    } else {
      span(class = "brand-lockup-fallback", icon("graduation-cap"))
    },
    tags$div(
      class = "brand-lockup-copy",
      tags$div("UIBS Bengaluru", class = "brand-lockup-title"),
      tags$div("United International Business School", class = "brand-lockup-sub")
    )
  )
}

read_image_raster <- function(path) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))

  tryCatch({
    if (ext == "png") {
      readPNG(path)
    } else if (ext %in% c("jpg", "jpeg") && requireNamespace("jpeg", quietly = TRUE)) {
      jpeg::readJPEG(path)
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

fit_text_cex <- function(label, max_width, base_cex = 1, min_cex = 0.4, font = 1) {
  label <- paste(label, collapse = " ")
  width <- strwidth(label, cex = base_cex, units = "user", font = font)
  if (!is.finite(width) || width <= 0) return(base_cex)
  max(min_cex, min(base_cex, base_cex * (max_width / width)))
}

draw_image_cover <- function(img, xleft, ybottom, xright, ytop) {
  if (is.null(img)) return(invisible(FALSE))

  dims <- dim(img)
  if (length(dims) < 2 || any(!is.finite(dims[1:2])) || dims[1] <= 0 || dims[2] <= 0) {
    return(invisible(FALSE))
  }

  box_ratio <- (xright - xleft) / (ytop - ybottom)
  img_ratio <- dims[2] / dims[1]

  if (img_ratio > box_ratio) {
    crop_width <- max(1, round(dims[1] * box_ratio))
    start_col <- max(1, floor((dims[2] - crop_width) / 2) + 1)
    end_col <- min(dims[2], start_col + crop_width - 1)
    img <- img[, start_col:end_col, , drop = FALSE]
  } else if (img_ratio < box_ratio) {
    crop_height <- max(1, round(dims[2] / box_ratio))
    start_row <- max(1, floor((dims[1] - crop_height) / 2) + 1)
    end_row <- min(dims[1], start_row + crop_height - 1)
    img <- img[start_row:end_row, , , drop = FALSE]
  }

  rasterImage(img, xleft, ybottom, xright, ytop, interpolate = TRUE)
  invisible(TRUE)
}

setup_card_plot <- function() {
  op <- par(no.readonly = TRUE)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", family = "sans")
  plot.new()
  plot.window(xlim = c(0, 100), ylim = c(0, 100))
  invisible(op)
}

compute_results <- function(marks, subject_info, prev_cgpa = 0) {
  marks <- as.numeric(marks)
  marks[is.na(marks)] <- 0

  pass_marks <- get_pass_marks(subject_info)
  fail_flags <- marks < pass_marks
  total <- sum(marks)
  total_max <- get_total_max(subject_info)
  percentage <- if (total_max > 0) (total / total_max) * 100 else 0
  sgpa <- min(round(percentage / 10, 2), 10)
  prev_cgpa <- ifelse(is.na(prev_cgpa), 0, prev_cgpa)
  cgpa <- if (prev_cgpa > 0) round((prev_cgpa + sgpa) / 2, 2) else sgpa

  grade <- if (any(fail_flags)) {
    "F"
  } else if (percentage >= 85) {
    "A+"
  } else if (percentage >= 70) {
    "A"
  } else if (percentage >= 55) {
    "B"
  } else {
    "C"
  }

  list(
    marks = marks,
    total = round(total, 2),
    percentage = round(percentage, 2),
    sgpa = sgpa,
    cgpa = cgpa,
    grade = grade,
    fail_flags = fail_flags
  )
}

build_marksheet <- function(student_row) {
  snapshot <- student_snapshot(student_row)
  subject_info <- snapshot$subject_info
  marks <- snapshot$marks
  attendance_details <- student_attendance_details(student_row, subject_info)
  internal_details <- student_internal_details(student_row, subject_info)
  external_marks <- sanitize_marks(student_row[1, subject_info$code, drop = TRUE], data.frame(max_mark = external_max_mark(subject_info$max_mark)))
  data.frame(
    PaperCode = subject_info$paper_code,
    Subject = subject_info$subject,
    Max = subject_info$max_mark,
    Pass = get_pass_marks(subject_info),
    Credits = subject_info$credits,
    External = external_marks,
    Internal = internal_details$InternalScore,
    Score = marks,
    ClassesTaken = attendance_details$ClassesTaken,
    ClassesAttended = attendance_details$ClassesAttended,
    AttendancePct = attendance_details$AttendancePct,
    Status = ifelse(marks >= get_pass_marks(subject_info), "Pass", "Reappear"),
    stringsAsFactors = FALSE
  )
}

ui <- dashboardPage(
  skin = "black",
  title = "UIBS Bengaluru | Academic Suite",
  header = dashboardHeader(
    title = brand_title_ui(),
    titleWidth = 360
  ),
  sidebar = dashboardSidebar(
    width = 280,
    uiOutput("sidebar_ui")
  ),
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        :root {
          --uibs-maroon: #5b1f41;
          --uibs-orange: #ef6a3a;
          --uibs-gold: #d4a84f;
          --uibs-slate: #0f172a;
          --uibs-bg: #f3f5f8;
          --uibs-card: #ffffff;
        }
        body, .content-wrapper, .right-side {
          background: linear-gradient(180deg, #eef2f7 0%, #f9fbfd 100%) !important;
        }
        .main-header .logo {
          background: var(--uibs-maroon) !important;
          color: white !important;
          font-weight: 800;
          letter-spacing: 0.4px;
          height: 70px;
          display: flex;
          align-items: center;
          justify-content: flex-start;
          padding: 0 16px !important;
        }
        .main-header .navbar {
          background: var(--uibs-maroon) !important;
        }
        .brand-lockup {
          display: flex;
          align-items: center;
          gap: 12px;
          height: 100%;
          color: white;
        }
        .brand-lockup-img {
          height: 48px;
          width: auto;
          object-fit: contain;
          background: white;
          border-radius: 12px;
          padding: 4px 8px;
        }
        .brand-lockup-fallback {
          width: 48px;
          height: 48px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          border-radius: 14px;
          background: rgba(255,255,255,0.12);
          font-size: 24px;
        }
        .brand-lockup-copy {
          display: flex;
          flex-direction: column;
          line-height: 1.05;
        }
        .brand-lockup-title {
          font-size: 18px;
          font-weight: 900;
        }
        .brand-lockup-sub {
          font-size: 11px;
          opacity: 0.86;
          font-weight: 700;
          letter-spacing: 0.3px;
        }
        .main-sidebar {
          background: linear-gradient(180deg, #441632 0%, #5b1f41 55%, #6b2b4e 100%) !important;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li:hover > a {
          border-left-color: var(--uibs-gold) !important;
          background: rgba(255,255,255,0.08) !important;
        }
        .content {
          padding: 22px;
        }
        .box {
          border-top: 4px solid var(--uibs-maroon);
          border-radius: 18px;
          box-shadow: 0 14px 35px rgba(15, 23, 42, 0.08);
          background: var(--uibs-card);
        }
        .small-box {
          border-radius: 18px;
          overflow: hidden;
          box-shadow: 0 12px 28px rgba(15, 23, 42, 0.08);
        }
        .small-box h3 {
          font-size: 28px;
          font-weight: 800;
        }
        .attendance-grid {
          display: grid;
          grid-template-columns: minmax(220px, 1.7fr) minmax(150px, 1fr) minmax(150px, 1fr) 72px;
          column-gap: 24px;
          row-gap: 14px;
          align-items: end;
          margin-top: 14px;
        }
        .attendance-row {
          display: contents;
        }
        .attendance-subject {
          align-self: center;
          font-weight: 800;
          line-height: 1.35;
          color: #1f2937;
          word-break: normal;
          overflow-wrap: anywhere;
        }
        .attendance-grid .form-group,
        .attendance-grid .shiny-input-container {
          width: 100%;
          margin-bottom: 0;
        }
        .attendance-grid .control-label {
          font-weight: 800;
          color: #1f2937;
          white-space: nowrap;
        }
        .attendance-grid .form-control {
          min-height: 42px;
        }
        .attendance-percent {
          align-self: center;
          justify-self: end;
          min-width: 56px;
          text-align: right;
          font-weight: 900;
          color: #64748b;
          font-size: 16px;
        }
        @media (max-width: 900px) {
          .attendance-grid {
            grid-template-columns: 1fr;
            row-gap: 8px;
          }
          .attendance-row {
            display: grid;
            grid-template-columns: 1fr;
            row-gap: 8px;
            padding: 12px 0;
            border-bottom: 1px solid #e5e7eb;
          }
          .attendance-percent {
            justify-self: start;
            text-align: left;
          }
        }
        .hero-panel {
          background: linear-gradient(135deg, #4a1937 0%, #7c2f59 55%, #ef6a3a 100%);
          color: white;
          border-radius: 24px;
          padding: 24px 26px;
          margin-bottom: 18px;
          box-shadow: 0 18px 40px rgba(91, 31, 65, 0.22);
        }
        .hero-title {
          font-size: 28px;
          font-weight: 900;
          margin-bottom: 8px;
        }
        .hero-sub {
          opacity: 0.88;
          margin-bottom: 0;
        }
        .uibs-auth-open .main-header,
        .uibs-auth-open .main-sidebar,
        .uibs-auth-open .left-side,
        .uibs-auth-open .main-footer {
          display: none !important;
        }
        .uibs-auth-open .content-wrapper,
        .uibs-auth-open .right-side {
          margin-left: 0 !important;
          min-height: 100vh !important;
        }
        .uibs-auth-open .content {
          padding: 0 !important;
        }
        .login-wrapper {
          position: fixed;
          inset: 0;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 24px;
          overflow-y: auto;
          background:
            radial-gradient(circle at top left, rgba(239,106,58,0.28), transparent 25%),
            radial-gradient(circle at bottom right, rgba(212,168,79,0.22), transparent 20%),
            linear-gradient(135deg, #210b18 0%, #4a1937 45%, #7c2f59 100%);
          z-index: 9999;
        }
        .login-card {
          width: min(720px, calc(100vw - 48px));
          background: rgba(255,255,255,0.12);
          backdrop-filter: blur(18px);
          border: 1px solid rgba(255,255,255,0.18);
          border-radius: 30px;
          padding: 34px 36px;
          color: white;
          box-shadow: 0 18px 50px rgba(0,0,0,0.25);
          max-height: calc(100vh - 48px);
          overflow-y: auto;
        }
        .auth-card-compact {
          width: min(860px, calc(100vw - 48px));
        }
        .auth-card-wide {
          width: min(1120px, calc(100vw - 48px));
        }
        .auth-shell {
          display: grid;
          grid-template-columns: minmax(260px, 320px) minmax(0, 1fr);
          gap: 30px;
          align-items: start;
        }
        .auth-aside {
          padding-right: 28px;
          border-right: 1px solid rgba(255,255,255,0.14);
        }
        .auth-main {
          min-width: 0;
        }
        .login-brand {
          text-align: center;
          margin-bottom: 18px;
        }
        .auth-aside .login-brand {
          text-align: left;
          margin-bottom: 22px;
        }
        .login-brand-img {
          max-width: 100%;
          width: 320px;
          max-height: 132px;
          object-fit: contain;
          background: rgba(255,255,255,0.96);
          border-radius: 22px;
          padding: 12px 18px;
          box-shadow: 0 14px 36px rgba(15, 23, 42, 0.2);
        }
        .login-brand-fallback {
          font-size: 58px;
          margin-bottom: 10px;
        }
        .login-card .form-group label {
          color: white;
          font-weight: 700;
        }
        .login-card .form-control,
        .login-card .selectize-input,
        .login-card .selectize-control.single .selectize-input.input-active {
          border-radius: 12px;
          min-height: 48px;
        }
        .login-card .row {
          margin-left: -8px;
          margin-right: -8px;
        }
        .login-card [class*='col-'] {
          padding-left: 8px;
          padding-right: 8px;
        }
        .login-card .shiny-input-container,
        .login-card .input-group,
        .login-card .selectize-control {
          width: 100% !important;
        }
        .auth-kicker {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          padding: 8px 12px;
          border-radius: 999px;
          background: rgba(255,255,255,0.12);
          font-size: 12px;
          font-weight: 800;
          letter-spacing: 0.4px;
          text-transform: uppercase;
          margin-bottom: 16px;
        }
        .auth-title {
          font-size: 40px;
          line-height: 1.05;
          font-weight: 900;
          margin: 0 0 10px 0;
        }
        .auth-copy {
          color: rgba(255,255,255,0.84);
          font-size: 16px;
          line-height: 1.6;
          margin-bottom: 18px;
        }
        .auth-points {
          list-style: none;
          padding: 0;
          margin: 0;
          display: grid;
          gap: 12px;
        }
        .auth-points li {
          position: relative;
          padding-left: 20px;
          color: rgba(255,255,255,0.88);
          line-height: 1.5;
        }
        .auth-points li::before {
          content: '';
          position: absolute;
          left: 0;
          top: 10px;
          width: 8px;
          height: 8px;
          border-radius: 999px;
          background: var(--uibs-orange);
          box-shadow: 0 0 0 4px rgba(239,106,58,0.18);
        }
        .auth-panel {
          background: rgba(255,255,255,0.07);
          border: 1px solid rgba(255,255,255,0.10);
          border-radius: 24px;
          padding: 20px;
        }
        .auth-panel-title {
          font-size: 24px;
          font-weight: 900;
          margin: 0 0 8px 0;
        }
        .auth-panel-copy {
          color: rgba(255,255,255,0.78);
          margin-bottom: 18px;
        }
        .auth-section {
          background: rgba(255,255,255,0.05);
          border: 1px solid rgba(255,255,255,0.08);
          border-radius: 20px;
          padding: 18px;
          margin-bottom: 16px;
        }
        .auth-section-title {
          font-size: 15px;
          font-weight: 900;
          letter-spacing: 0.3px;
          text-transform: uppercase;
          margin-bottom: 14px;
          color: rgba(255,255,255,0.9);
        }
        .auth-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 16px 18px;
          align-items: start;
        }
        .auth-grid-3 {
          grid-template-columns: repeat(3, minmax(0, 1fr));
        }
        .auth-span-2 {
          grid-column: span 2;
        }
        .auth-span-3 {
          grid-column: span 3;
        }
        .auth-actions {
          display: flex;
          align-items: center;
          gap: 14px;
          margin-top: 18px;
          flex-wrap: wrap;
        }
        .auth-actions .btn-login {
          min-width: 220px;
        }
        .auth-switch {
          margin-top: 18px;
          display: flex;
          gap: 10px;
          align-items: center;
          flex-wrap: wrap;
          color: rgba(255,255,255,0.88);
        }
        .auth-switch a {
          color: white !important;
          font-weight: 800;
        }
        .auth-note {
          color: rgba(255,255,255,0.72);
          font-size: 13px;
          margin-top: 6px;
        }
        .auth-main .form-group {
          margin-bottom: 0;
        }
        .auth-main .form-control,
        .auth-main .selectize-input,
        .auth-main .selectize-control.single .selectize-input.input-active {
          background: rgba(255,255,255,0.98);
          border: 1px solid rgba(255,255,255,0.28);
          color: #1f2937;
          min-height: 52px;
          padding-top: 12px;
          padding-bottom: 12px;
        }
        .auth-main .form-control::placeholder {
          color: #94a3b8;
        }
        .auth-main .input-group-btn .btn,
        .auth-main .btn-default {
          min-height: 52px;
        }
        .auth-main .control-label {
          margin-bottom: 8px;
        }
        .auth-main .help-block {
          color: rgba(255,255,255,0.72);
          margin-top: 6px;
          margin-bottom: 0;
        }
        .auth-mode-login .auth-grid {
          grid-template-columns: 1fr 1fr;
        }
        .auth-mode-login .auth-grid .auth-span-2 {
          grid-column: span 2;
        }
        @media (max-width: 900px) {
          .auth-shell {
            grid-template-columns: 1fr;
            gap: 22px;
          }
          .auth-aside {
            padding-right: 0;
            padding-bottom: 20px;
            border-right: none;
            border-bottom: 1px solid rgba(255,255,255,0.14);
          }
          .auth-grid,
          .auth-grid-3,
          .auth-mode-login .auth-grid {
            grid-template-columns: 1fr;
          }
          .auth-span-2,
          .auth-span-3 {
            grid-column: auto;
          }
          .auth-title {
            font-size: 32px;
          }
        }
        .profile-shell {
          display: flex;
          gap: 18px;
          align-items: center;
          flex-wrap: wrap;
          text-align: left;
        }
        .profile-shell-compact {
          gap: 14px;
          align-items: center;
        }
        .profile-img {
          width: 132px;
          height: 132px;
          object-fit: cover;
          border-radius: 24px;
          box-shadow: 0 12px 28px rgba(15, 23, 42, 0.12);
          flex-shrink: 0;
        }
        .profile-fallback {
          width: 132px;
          height: 132px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          border-radius: 24px;
          background: linear-gradient(135deg, #e2e8f0 0%, #cbd5e1 100%);
          color: #334155;
          font-size: 42px;
          font-weight: 900;
          line-height: 1;
          flex-shrink: 0;
        }
        .profile-fallback-sm {
          width: 72px;
          height: 72px;
          border-radius: 18px;
          font-size: 28px;
        }
        .profile-copy {
          flex: 1;
          min-width: 220px;
        }
        .pending-staff-picker .radio {
          margin: 0;
        }
        .pending-staff-picker .radio label {
          display: block;
          padding: 12px 14px;
          margin-bottom: 10px;
          border-radius: 14px;
          border: 1px solid #e2e8f0;
          background: #f8fafc;
          font-weight: 700;
          color: #334155;
        }
        .pending-staff-picker .radio input[type='radio'] {
          margin-top: 3px;
        }
        .pending-empty {
          padding: 12px 14px;
          border-radius: 14px;
          background: #f8fafc;
          color: #64748b;
          font-weight: 700;
        }
        .manager-form-section {
          padding: 16px 0 4px 0;
          border-bottom: 1px solid #e2e8f0;
          margin-bottom: 14px;
        }
        .manager-form-section:last-child {
          border-bottom: none;
          margin-bottom: 0;
        }
        .manager-form-title {
          font-size: 13px;
          font-weight: 900;
          letter-spacing: 0.4px;
          text-transform: uppercase;
          color: #64748b;
          margin-bottom: 14px;
        }
        .manager-form-actions {
          display: flex;
          gap: 12px;
          flex-wrap: wrap;
          align-items: center;
        }
        .manager-form-actions .btn-uibs {
          min-width: 190px;
        }
        @media (max-width: 1180px) {
          .main-header .logo {
            width: 280px !important;
            padding: 0 12px !important;
          }
          .main-header .navbar {
            margin-left: 280px !important;
          }
          .main-sidebar,
          .left-side {
            width: 280px !important;
          }
          .content-wrapper,
          .right-side,
          .main-footer {
            margin-left: 280px !important;
          }
          .brand-lockup-copy {
            min-width: 0;
          }
          .brand-lockup-title {
            font-size: 15px;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }
          .brand-lockup-sub {
            font-size: 10px;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }
        }
        @media (max-width: 860px) {
          .main-header .logo {
            width: 100% !important;
            max-width: 100vw !important;
          }
          .main-header .navbar {
            margin-left: 0 !important;
          }
          .content-wrapper,
          .right-side,
          .main-footer {
            margin-left: 0 !important;
          }
          .content {
            padding: 14px;
          }
          .brand-lockup {
            gap: 10px;
          }
          .brand-lockup-img,
          .brand-lockup-fallback {
            height: 42px;
            width: 42px;
            border-radius: 12px;
            padding: 4px;
          }
          .brand-lockup-title {
            font-size: 12px;
          }
          .brand-lockup-sub {
            display: none;
          }
          .sidebar-menu > li > a {
            padding-top: 14px;
            padding-bottom: 14px;
          }
          .profile-shell {
            align-items: flex-start;
          }
          .profile-copy {
            min-width: 0;
          }
        }
        .btn-login, .btn-uibs {
          border: none;
          border-radius: 12px;
          background: linear-gradient(135deg, #ef6a3a 0%, #ff8359 100%);
          color: white;
          font-weight: 800;
          padding: 12px 18px;
          box-shadow: 0 12px 28px rgba(239,106,58,0.25);
        }
        .info-chip {
          display: inline-block;
          padding: 7px 12px;
          margin: 0 8px 8px 0;
          border-radius: 999px;
          background: #fff3ed;
          color: #9a3412;
          font-weight: 700;
        }
        .profile-shell {
          display: flex;
          gap: 18px;
          align-items: center;
          flex-wrap: wrap;
          text-align: left;
          padding: 8px 6px 18px;
        }
        .profile-img {
          width: 132px;
          height: 132px;
          object-fit: cover;
          border-radius: 22px;
          border: 6px solid #fff;
          box-shadow: 0 12px 30px rgba(15, 23, 42, 0.12);
          background: #e2e8f0;
          flex-shrink: 0;
        }
        .profile-fallback {
          width: 132px;
          height: 132px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          line-height: 1;
          border-radius: 22px;
          background: linear-gradient(135deg, #e2e8f0 0%, #cbd5e1 100%);
          color: #475569;
          font-size: 42px;
          font-weight: 800;
          flex-shrink: 0;
        }
        .profile-copy {
          flex: 1;
          min-width: 220px;
        }
        .stat-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 12px;
          margin-top: 16px;
        }
        .stat-card {
          background: #f8fafc;
          border: 1px solid #e2e8f0;
          border-radius: 16px;
          padding: 14px;
          text-align: center;
        }
        .stat-val {
          display: block;
          font-size: 22px;
          font-weight: 900;
          color: var(--uibs-maroon);
        }
        .stat-lbl {
          display: block;
          margin-top: 4px;
          color: #64748b;
          text-transform: uppercase;
          font-size: 11px;
          font-weight: 800;
          letter-spacing: 0.6px;
        }
        .notice-card {
          border-left: 4px solid var(--uibs-orange);
          background: #fffaf7;
          border-radius: 16px;
          padding: 16px;
          margin-bottom: 12px;
        }
        .notice-title {
          font-size: 16px;
          font-weight: 900;
          color: var(--uibs-slate);
          margin-bottom: 4px;
        }
        .notice-meta {
          color: #64748b;
          font-size: 12px;
          font-weight: 700;
          margin-bottom: 8px;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('uibs-force-tab', function(message) {
          if (!message || !message.tab) return;
          var targetId = 'shiny-tab-' + message.tab;
          var panes = document.querySelectorAll('.content-wrapper .tab-pane');
          panes.forEach(function(pane) {
            pane.classList.remove('active');
            pane.style.display = 'none';
          });
          var targetPane = document.getElementById(targetId);
          if (targetPane) {
            targetPane.classList.add('active');
            targetPane.style.display = 'block';
          }
        });
      "))
    ),
    uiOutput("body_ui")
  )
)

server <- function(input, output, session) {
  logged_in <- reactiveVal(FALSE)
  user_role <- reactiveVal(NULL)
  user_reg <- reactiveVal(NULL)
  current_staff_designation <- reactiveVal("")
  active_tab <- reactiveVal("admin_dash")
  auth_mode <- reactiveVal("login")
  auth_result_title <- reactiveVal("")
  auth_result_body <- reactiveVal("")
  students_refresh <- reactiveVal(0)
  announcements_refresh <- reactiveVal(0)
  users_refresh <- reactiveVal(0)
  daily_attendance_refresh <- reactiveVal(0)
  timetable_refresh <- reactiveVal(0)

  default_tab_for_role <- function(role_value) {
    if (is_admin_user(role_value)) {
      "admin_dash"
    } else if (identical(role_value, "faculty")) {
      "staff_dash"
    } else {
      "student_dash"
    }
  }

  go_to_default_tab <- function(role_value) {
    target_tab <- default_tab_for_role(role_value)
    active_tab(target_tab)
    session$onFlushed(function() {
      try(updateTabItems(session, "tabs", target_tab), silent = TRUE)
    }, once = TRUE)
    invisible(target_tab)
  }

  safe_write_csv <- function(data, path, refresh_counter, label) {
    last_error <- "Unknown write error."

    for (attempt in seq_len(6)) {
      temp_path <- tempfile(
        pattern = paste0(tools::file_path_sans_ext(basename(path)), "_"),
        tmpdir = dirname(path),
        fileext = ".csv"
      )

      success <- tryCatch({
        write.csv(data, temp_path, row.names = FALSE)
        copied <- suppressWarnings(file.copy(temp_path, path, overwrite = TRUE, copy.mode = TRUE))
        if (!isTRUE(copied)) {
          stop("The destination file is locked or temporarily unavailable.")
        }
        TRUE
      }, error = function(e) {
        last_error <<- conditionMessage(e)
        FALSE
      })

      if (file.exists(temp_path)) unlink(temp_path, force = TRUE)

      if (success) {
        refresh_counter(isolate(refresh_counter()) + 1)
        return(TRUE)
      }

      Sys.sleep(0.2 * attempt)
    }

    showNotification(
      paste0(
        "Unable to save ", label,
        ". Close any open CSV/Excel window using this file and try again."
      ),
      type = "error",
      duration = 8
    )
    message(paste("CSV write failed for", path, ":", last_error))
    FALSE
  }

  write_students_db <- function(students) {
    students <- ensure_schema(students, students_template)
    invisible(safe_write_csv(students, STUDENTS_DB, students_refresh, "student records"))
  }

  write_announcements_db <- function(notices) {
    notices <- ensure_schema(notices, announcements_template)
    invisible(safe_write_csv(notices, ANNOUNCEMENTS_DB, announcements_refresh, "announcements"))
  }

  write_users_db <- function(users) {
    users <- normalize_user_records(ensure_schema(users, users_template))
    invisible(safe_write_csv(users, USERS_DB, users_refresh, "user accounts"))
  }

  write_daily_attendance_db <- function(records) {
    records <- ensure_schema(records, daily_attendance_template)
    invisible(safe_write_csv(records, DAILY_ATTENDANCE_DB, daily_attendance_refresh, "attendance records"))
  }

  write_timetable_db <- function(records) {
    records <- ensure_schema(records, timetable_template)
    invisible(safe_write_csv(records, TIMETABLE_DB, timetable_refresh, "time table records"))
  }

  students_file_reader <- reactiveFileReader(
    500, session, STUDENTS_DB,
    function(path) ensure_schema(read_safe_csv(path, students_template), students_template)
  )
  announcements_file_reader <- reactiveFileReader(
    500, session, ANNOUNCEMENTS_DB,
    function(path) ensure_schema(read_safe_csv(path, announcements_template), announcements_template)
  )
  users_file_reader <- reactiveFileReader(
    500, session, USERS_DB,
    function(path) normalize_user_records(ensure_schema(read_safe_csv(path, users_template), users_template))
  )
  daily_attendance_file_reader <- reactiveFileReader(
    500, session, DAILY_ATTENDANCE_DB,
    function(path) ensure_schema(read_safe_csv(path, daily_attendance_template), daily_attendance_template)
  )
  timetable_file_reader <- reactiveFileReader(
    500, session, TIMETABLE_DB,
    function(path) ensure_schema(read_safe_csv(path, timetable_template), timetable_template)
  )

  get_students <- reactive({
    students_refresh()
    students_file_reader()
  })

  get_announcements <- reactive({
    announcements_refresh()
    announcements_file_reader()
  })

  get_users <- reactive({
    users_refresh()
    users_file_reader()
  })

  get_daily_attendance <- reactive({
    daily_attendance_refresh()
    daily_attendance_file_reader()
  })

  get_timetable <- reactive({
    timetable_refresh()
    timetable_file_reader()
  })

  selected_student_semester <- reactiveVal(NULL)

  current_student <- reactive({
    req(user_reg())
    db <- get_students()
    semester_records <- student_semester_records(db, user_reg())
    if (nrow(semester_records) == 0) return(semester_records)
    chosen_semester <- selected_student_semester()
    if (!is.null(chosen_semester) && nzchar(chosen_semester)) {
      chosen <- semester_records[semester_records$Semester == chosen_semester, , drop = FALSE]
      if (nrow(chosen) > 0) return(chosen[1, , drop = FALSE])
    }
    semester_records[1, , drop = FALSE]
  })

  current_user_account <- reactive({
    req(user_reg())
    if (is_admin_user(user_role())) {
      return(data.frame(
        RegNo = "CSB",
        Name = "Admin",
        Email = "admin@uibsblr.com",
        StaffRole = "Admin",
        Photo = "",
        CRAccess = "No",
        CRDept = "",
        CRYear = "",
        CRSemester = "",
        CRAssignedBy = "",
        CRAssignedByRole = "",
        CRAssignedOn = "",
        AddressLine1 = "",
        AddressLine2 = "",
        City = "",
        State = "",
        Pincode = "",
        stringsAsFactors = FALSE
      ))
    }
    users <- get_users()
    rows <- users[users$RegNo == user_reg(), , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    rows <- ensure_schema(rows[1, , drop = FALSE], users_template)
    if (!nzchar(rows$Name[1])) rows$Name[1] <- rows$RegNo[1]
    if (identical(user_role(), "faculty") && !nzchar(rows$StaffRole[1])) {
      rows$StaffRole[1] <- if (nzchar(current_staff_designation())) current_staff_designation() else "Faculty"
    }
    if (identical(user_role(), "student")) {
      latest_row <- latest_student_record(get_students(), user_reg())
      if (nrow(latest_row) > 0) {
        if (!nzchar(rows$Name[1]) || identical(rows$Name[1], rows$RegNo[1])) rows$Name[1] <- latest_row$Name[1]
        if (!nzchar(rows$Dept[1])) rows$Dept[1] <- latest_row$Dept[1]
        if (!nzchar(rows$Year[1])) rows$Year[1] <- latest_row$Year[1]
        if (!nzchar(rows$Photo[1])) rows$Photo[1] <- latest_row$Photo[1]
      }
    }
    rows
  })

  observe({
    db <- get_students()
    student_choices <- c("SELECT" = "", unique(db$RegNo))
    current_dash_student <- if (!is.null(input$dash_student) && input$dash_student %in% student_choices) input$dash_student else ""
    current_info_reg <- if (!is.null(input$info_reg) && input$info_reg %in% student_choices) input$info_reg else ""
    current_internal_reg <- if (!is.null(input$internal_reg) && input$internal_reg %in% student_choices) input$internal_reg else ""
    current_ops_reg <- if (!is.null(input$ops_reg) && input$ops_reg %in% student_choices) input$ops_reg else ""
    current_fee_reg <- if (!is.null(input$fee_reg) && input$fee_reg %in% student_choices) input$fee_reg else ""

    updateSelectInput(session, "dash_student", choices = student_choices, selected = current_dash_student)
    updateSelectInput(session, "info_reg", choices = student_choices, selected = current_info_reg)
    updateSelectInput(session, "internal_reg", choices = student_choices, selected = current_internal_reg)
    updateSelectInput(session, "ops_reg", choices = student_choices, selected = current_ops_reg)
    updateSelectInput(session, "fee_reg", choices = student_choices, selected = current_fee_reg)
  })

  admin_summary <- reactive({
    db <- get_students()
    active <- db[db$Grade != "N/A" & nzchar(db$Grade), , drop = FALSE]
    list(
      total_students = nrow(db),
      pass_count = sum(active$Grade != "F", na.rm = TRUE),
      fail_count = sum(active$Grade == "F", na.rm = TRUE),
      avg_pct = if (nrow(active) > 0) mean(active$Percentage, na.rm = TRUE) else 0
    )
  })

  filtered_registry <- reactive({
    db <- get_students()
    if (!is.null(input$filter_dept) && nzchar(input$filter_dept)) {
      db <- db[db$Dept == input$filter_dept, , drop = FALSE]
    }
    if (!is.null(input$filter_year) && nzchar(input$filter_year)) {
      db <- db[db$Year == input$filter_year, , drop = FALSE]
    }
    if (!is.null(input$filter_grade) && nzchar(input$filter_grade)) {
      db <- db[db$Grade == input$filter_grade, , drop = FALSE]
    }
    db
  })

  selected_subject_info <- reactive({
    if (is.null(input$res_dept) || !nzchar(input$res_dept) || is.null(input$res_semester) || !nzchar(input$res_semester)) {
      return(NULL)
    }
    get_subject_info(input$res_dept, input$res_semester)
  })

  grading_target_record <- reactive({
    if (is.null(input$res_id) || !nzchar(input$res_id) || is.null(input$res_semester) || !nzchar(input$res_semester)) return(NULL)
    db <- get_students()
    rows <- db[db$RegNo == input$res_id & db$Semester == input$res_semester, , drop = FALSE]
    if (nrow(rows) == 0) return(latest_student_record(db, input$res_id))
    rows[1, , drop = FALSE]
  })

  calc_preview <- reactive({
    subject_info <- selected_subject_info()
    if (is.null(subject_info) || nrow(subject_info) == 0) return(NULL)
    external_marks <- sapply(seq_len(nrow(subject_info)), function(i) input[[paste0("grade_", subject_info$code[i])]])
    base_row <- grading_target_record()
    if (is.null(base_row) || nrow(base_row) == 0) {
      combined_marks <- sanitize_marks(external_marks, data.frame(max_mark = external_max_mark(subject_info$max_mark)))
    } else {
      working_row <- base_row
      working_row[1, subject_info$code] <- sanitize_marks(external_marks, data.frame(max_mark = external_max_mark(subject_info$max_mark)))
      combined_marks <- student_total_marks(working_row, subject_info)
    }
    prev_cgpa <- if (is.null(input$grade_prev) || is.na(input$grade_prev)) 0 else input$grade_prev
    compute_results(combined_marks, subject_info, prev_cgpa)
  })

  announcements_for_user <- reactive({
    notices <- get_announcements()
    notices <- notices[order(notices$PostedOn, decreasing = TRUE), , drop = FALSE]
    if (is_staff_role(user_role())) return(notices)
    notices[notices$Audience %in% c("All", "Students"), , drop = FALSE]
  })

  observe({
    req(user_reg())
    semesters <- unique(student_semester_records(get_students(), user_reg())$Semester)
    semesters <- semesters[nzchar(semesters)]
    if (length(semesters) == 0) {
      selected_student_semester(NULL)
    } else if (is.null(selected_student_semester()) || !selected_student_semester() %in% semesters) {
      selected_student_semester(semesters[1])
    }
  })

  observeEvent(input$res_dept, {
    semesters <- get_semester_choices(input$res_dept)
    selected <- if (!is.null(input$res_semester) && input$res_semester %in% semesters) input$res_semester else ""
    updateSelectInput(session, "res_semester", choices = c("SELECT" = "", semesters), selected = selected)
  }, ignoreInit = FALSE)

  observeEvent(list(input$res_dept, input$res_semester), {
    req(input$res_dept, input$res_semester)
    if (requires_language(input$res_dept, input$res_semester)) {
      selected <- if (!is.null(input$res_lang) && nzchar(input$res_lang)) input$res_lang else ""
      updateSelectInput(session, "res_lang", label = "Language 1", choices = c("SELECT" = "", LANG_OPTIONS), selected = selected)
    } else {
      updateSelectInput(session, "res_lang", label = "Language 1", choices = c("Not Applicable" = ""), selected = "")
    }
  }, ignoreInit = FALSE)

  observeEvent(input$tt_dept, {
    semesters <- get_semester_choices(input$tt_dept)
    selected <- if (!is.null(input$tt_semester) && input$tt_semester %in% semesters) input$tt_semester else ""
    updateSelectInput(session, "tt_semester", choices = c("SELECT" = "", semesters), selected = selected)
  }, ignoreInit = FALSE)

  dashboard_students <- reactive({
    db <- get_students()
    if (!is.null(input$dash_dept) && nzchar(input$dash_dept)) {
      db <- db[db$Dept == input$dash_dept, , drop = FALSE]
    }
    if (!is.null(input$dash_semester) && nzchar(input$dash_semester)) {
      db <- db[db$Semester == input$dash_semester, , drop = FALSE]
    }
    db
  })

  ops_student <- reactive({
    if (is.null(input$ops_reg) || !nzchar(input$ops_reg)) return(NULL)
    semester <- if (!is.null(input$ops_semester) && nzchar(input$ops_semester)) input$ops_semester else ""
    db <- get_students()
    if (nzchar(semester)) {
      rows <- db[db$RegNo == input$ops_reg & db$Semester == semester, , drop = FALSE]
      if (nrow(rows) > 0) return(rows[1, , drop = FALSE])
    }
    latest_student_record(db, input$ops_reg)
  })

  output$sidebar_ui <- renderUI({
    if (!logged_in()) return(NULL)
    attendance_account <- current_user_account()
    default_tab <- if (is_admin_user(user_role())) {
      "admin_dash"
    } else if (identical(user_role(), "faculty")) {
      "staff_dash"
    } else {
      "student_dash"
    }
    current_tab <- active_tab()
    if (!nzchar(current_tab)) current_tab <- default_tab

    sidebarMenu(
      id = "tabs",
      selected = current_tab,
      if (is_admin_user(user_role())) {
        tagList(
          menuItem("Executive Dashboard", tabName = "admin_dash", icon = icon("chart-pie")),
          menuItem("Grading Studio", tabName = "admin_grading", icon = icon("marker")),
          menuItem("Internal Exams", tabName = "admin_internal", icon = icon("clipboard-check")),
          if (can_manage_daily_attendance(user_role(), current_staff_designation(), attendance_account)) menuItem("Daily Attendance", tabName = "staff_daily_attendance", icon = icon("calendar-check")),
          menuItem("Attendance Summary", tabName = "admin_ops", icon = icon("chart-bar")),
          menuItem("Fees Module", tabName = "admin_fees", icon = icon("money-check-alt")),
          menuItem("Student Info", tabName = "admin_students", icon = icon("users")),
          menuItem("Staff Management", tabName = "admin_staff", icon = icon("user-check")),
          menuItem("Time Table", tabName = "shared_timetable", icon = icon("calendar-alt")),
          menuItem("College Highlights", tabName = "shared_highlights", icon = icon("images")),
          menuItem("Announcements", tabName = "admin_notices", icon = icon("bullhorn"))
        )
      } else if (identical(user_role(), "faculty")) {
        tagList(
          menuItem("Staff Dashboard", tabName = "staff_dash", icon = icon("tachometer-alt")),
          menuItem("Grading Studio", tabName = "admin_grading", icon = icon("marker")),
          menuItem("Internal Exams", tabName = "admin_internal", icon = icon("clipboard-check")),
          if (can_manage_daily_attendance(user_role(), current_staff_designation(), attendance_account)) menuItem("Daily Attendance", tabName = "staff_daily_attendance", icon = icon("calendar-check")),
          menuItem("Time Table", tabName = "shared_timetable", icon = icon("calendar-alt")),
          menuItem("College Highlights", tabName = "shared_highlights", icon = icon("images")),
          menuItem("Staff Profile", tabName = "staff_profile", icon = icon("id-badge")),
          menuItem("Announcements", tabName = "admin_notices", icon = icon("bullhorn"))
        )
      } else {
        tagList(
          menuItem("Student Overview", tabName = "student_dash", icon = icon("id-card")),
          menuItem("Academic Record", tabName = "student_marks", icon = icon("book")),
          menuItem("Documents & Services", tabName = "student_services", icon = icon("file-download")),
          if (can_manage_daily_attendance(user_role(), "", attendance_account)) menuItem("CR Attendance", tabName = "staff_daily_attendance", icon = icon("calendar-check")),
          menuItem("Time Table", tabName = "shared_timetable", icon = icon("calendar-alt")),
          menuItem("College Highlights", tabName = "shared_highlights", icon = icon("images")),
          menuItem("Announcements", tabName = "student_notices", icon = icon("bell"))
        )
      },
      menuItem("Sign Out", tabName = "logout", icon = icon("power-off"))
    )
  })

  observeEvent(list(logged_in(), user_role()), {
    if (!logged_in()) return()
    go_to_default_tab(user_role())
  })

  observeEvent(input$tabs, {
    if (is.null(input$tabs) || !nzchar(input$tabs) || identical(input$tabs, "logout")) return()
    active_tab(input$tabs)
  })

  observeEvent(active_tab(), {
    if (!logged_in()) return()
    target_tab <- isolate(active_tab())
    if (!nzchar(target_tab)) return()
    session$onFlushed(function() {
      current_tab <- isolate(input$tabs)
      if (is.null(current_tab) || !identical(current_tab, target_tab)) {
        try(updateTabItems(session, "tabs", target_tab), silent = TRUE)
      }
      session$sendCustomMessage("uibs-force-tab", list(tab = target_tab))
    }, once = TRUE)
  }, ignoreInit = TRUE)

  output$body_ui <- renderUI({
    if (!logged_in()) {
      current_auth_mode <- auth_mode()
      auth_card_class <- if (identical(current_auth_mode, "login")) "login-card auth-card-compact" else "login-card auth-card-wide"
      auth_panel_title <- switch(
        current_auth_mode,
        "login" = "Access Your Account",
        "register" = "Create Your Institutional Account",
        "register_done" = auth_result_title(),
        "forgot" = "Reset Account Password",
        "Access Your Account"
      )
      auth_panel_copy <- switch(
        current_auth_mode,
        "login" = "Use your approved registration details to enter the academic suite.",
        "register" = "Complete every required section carefully so the account is created in the correct format.",
        "register_done" = auth_result_body(),
        "forgot" = "Reset access for student and approved staff accounts using the original registration ID.",
        ""
      )
      auth_points <- switch(
        current_auth_mode,
        "login" = c(
          "Students can view marks, attendance, documents and announcements.",
          "Approved staff can access teaching and administrative modules.",
          "Admin accounts manage approvals, records and institution-wide modules."
        ),
        "register" = c(
          "Registration ID must start with U19TO for students and staff.",
          "Staff must use an official @uibsblr.com email ID and upload a profile photo.",
          "Staff registration remains pending until admin approval."
        ),
        "register_done" = c(
          "Registration details have been saved successfully.",
          "Students can sign in once they return to the login page.",
          "Staff accounts remain locked until admin approval is completed."
        ),
        "forgot" = c(
          "Use the same role and registration ID used during account creation.",
          "Choose a new password carefully before updating access.",
          "Admin password remains institution controlled and is not reset here."
        ),
        character()
      )
      return(
        tagList(
          tags$script(HTML("document.body.classList.add('uibs-auth-open');")),
          div(
            class = "login-wrapper",
            div(
              class = auth_card_class,
              div(
                class = paste("auth-shell", paste0("auth-mode-", current_auth_mode)),
                div(
                  class = "auth-aside",
                  div(
                    class = "login-brand",
                    if (nzchar(app_logo_src())) {
                      tags$img(src = app_logo_src(), class = "login-brand-img")
                    } else {
                      div(class = "login-brand-fallback", icon("university"))
                    }
                  ),
                  div(class = "auth-kicker", icon("shield-alt"), "UIBS Academic Suite"),
                  h1(class = "auth-title", "UIBS Bengaluru"),
                  p(class = "auth-copy", "Integrated academic administration, student services and analytics."),
                  tags$ul(
                    class = "auth-points",
                    lapply(auth_points, function(point) tags$li(point))
                  )
                ),
                div(
                  class = "auth-main",
                  div(
                    class = "auth-panel",
                    div(class = "auth-panel-title", auth_panel_title),
                    p(class = "auth-panel-copy", auth_panel_copy),
                    uiOutput("auth_panel_ui"),
                    div(
                      class = "auth-switch",
                      if (identical(current_auth_mode, "login")) {
                        tagList(
                          actionLink("show_register", "Switch to Registration"),
                          tags$span("|"),
                          actionLink("show_forgot", "Forgot Password?")
                        )
                      } else if (identical(current_auth_mode, "register_done")) {
                        tagList(
                          actionLink("show_login", "Go to Login"),
                          tags$span("|"),
                          actionLink("show_register", "Register Another Account")
                        )
                      } else {
                        actionLink("show_login", "Back to Login")
                      }
                    )
                  )
                )
              )
            )
          )
        )
      )
    }

    student_choices <- c("SELECT" = "", isolate(get_students())$RegNo)
    current_tab <- isolate(active_tab())
    if (!nzchar(current_tab)) {
      current_tab <- default_tab_for_role(user_role())
    }
    activation_script <- sprintf(
      "(function() {
        var targetId = 'shiny-tab-%s';
        var activatePane = function() {
          var panes = document.querySelectorAll('.content-wrapper .tab-pane');
          panes.forEach(function(pane) {
            pane.classList.remove('active');
            pane.style.display = 'none';
          });
          var targetPane = document.getElementById(targetId);
          if (targetPane) {
            targetPane.classList.add('active');
            targetPane.style.display = 'block';
          }
        };
        setTimeout(activatePane, 0);
        setTimeout(activatePane, 120);
      })();",
      current_tab
    )

    tagList(
      tags$script(HTML("document.body.classList.remove('uibs-auth-open');")),
      tags$script(HTML(activation_script)),
      tabItems(
      tabItem(
        tabName = "admin_dash",
        div(
          class = "hero-panel",
          div(class = "hero-title", "Academic Command Center"),
          p(class = "hero-sub", "Live metrics for enrollment, results, attendance quality and student progression.")
        ),
        fluidRow(
          valueBoxOutput("box_total", 3),
          valueBoxOutput("box_avg", 3),
          valueBoxOutput("box_pass", 3),
          valueBoxOutput("box_fail", 3)
        ),
        fluidRow(
          box(
            title = "Analytics Filter",
            width = 12,
            fluidRow(
              column(3, selectInput("dash_semester", "Semester", choices = c("All" = "", SEMESTER_OPTIONS))),
              column(3, selectInput("dash_dept", "Department", choices = c("All" = "", DEPARTMENTS))),
              column(3, selectInput("dash_student", "Student Profile", choices = student_choices)),
              column(3, helpText("Charts below will adapt to the selected department and semester."))
            )
          )
        ),
        fluidRow(
          box(title = "Student Profile Preview", width = 12, uiOutput("admin_dash_student_profile"))
        ),
        fluidRow(
          box(title = "Department Distribution", width = 4, plotOutput("g_dept", height = 270)),
          box(title = "Year-wise Enrollment", width = 4, plotOutput("g_year", height = 270)),
          box(title = "Grade Composition", width = 4, plotOutput("g_grade_interactive", height = 300))
        ),
        fluidRow(
          box(title = "Overall Subject Performance", width = 8, plotOutput("g_sub_avgs", height = 320)),
          box(title = "Attendance Heat Snapshot", width = 4, plotOutput("g_attendance_band", height = 320))
        ),
        fluidRow(
          box(title = "Pass vs Fail", width = 6, plotOutput("g_result", height = 280)),
          box(title = "Top Failed Subjects", width = 6, plotOutput("g_fail_subjects", height = 280))
        )
      ),
      tabItem(
        tabName = "admin_grading",
        fluidRow(
          box(
            title = "Grade Publishing Studio",
            width = 12,
            fluidRow(
              column(3, textInput("res_id", "Student Registration ID")),
              column(2, actionButton("search_btn", "Load Student", class = "btn-uibs")),
              column(3, textInput("res_name", "Student Name")),
              column(2, selectInput("res_dept", "Department", choices = c("SELECT" = "", DEPARTMENTS))),
              column(2, selectInput("res_year", "Academic Year", choices = c("SELECT" = "", YEARS)))
            ),
            fluidRow(
              column(3, selectInput("res_semester", "Semester", choices = c("SELECT" = "", SEMESTER_OPTIONS))),
              column(3, selectInput("res_lang", "Language 1", choices = c("SELECT" = "", LANG_OPTIONS))),
              column(3, numericInput("grade_prev", "Previous CGPA", value = NA, min = 0, max = 10, step = 0.01)),
              column(3, textInput("grade_mentor", "Mentor"))
            ),
            hr(),
            uiOutput("grade_inputs_ui"),
            tags$div(
              style = "margin-top:22px; padding:22px; border-radius:18px; background:#f8fafc;",
              uiOutput("calc_box"),
              br(),
              actionButton("save_btn", "Publish Official Grade", icon = icon("cloud-upload-alt"), class = "btn-uibs")
            )
          )
        )
      ),
      tabItem(
        tabName = "admin_students",
        fluidRow(
          box(
            title = "Student Information Form",
            width = 7,
            div(
              class = "manager-form-section",
              div(class = "manager-form-title", "Student Lookup"),
              fluidRow(
                column(5, selectInput("info_reg", "Student", choices = student_choices)),
                column(7, textInput("info_name", "Student Name"))
              )
            ),
            div(
              class = "manager-form-section",
              div(class = "manager-form-title", "Academic Details"),
              fluidRow(
                column(4, selectInput("info_dept", "Department", choices = c("SELECT" = "", DEPARTMENTS))),
                column(4, selectInput("info_year", "Academic Year", choices = c("SELECT" = "", YEARS))),
                column(4, textInput("info_mentor", "Mentor"))
              )
            ),
            div(
              class = "manager-form-section",
              div(class = "manager-form-title", "Address Details"),
              textInput("info_address1", "House / Street"),
              textInput("info_address2", "Area / Landmark"),
              fluidRow(
                column(4, textInput("info_city", "City")),
                column(4, textInput("info_state", "State")),
                column(4, textInput("info_pincode", "Pincode"))
              )
            ),
            div(
              class = "manager-form-section",
              div(class = "manager-form-title", "Profile Update"),
              fluidRow(
                column(7, fileInput("info_photo", "Profile Picture", accept = c("image/png", "image/jpeg"))),
                column(5, div(style = "padding-top:25px;", actionButton("save_student_info", "Update Student Info", class = "btn-uibs")))
              )
            )
          ),
          box(
            title = "Student Profile Preview",
            width = 5,
            uiOutput("admin_student_profile")
          )
        ),
        fluidRow(
          box(
            title = "Registry Filters",
            width = 12,
            fluidRow(
              column(3, selectInput("filter_dept", "Department", choices = c("All" = "", DEPARTMENTS))),
              column(3, selectInput("filter_year", "Year", choices = c("All" = "", YEARS))),
              column(3, selectInput("filter_grade", "Grade", choices = c("All" = "", "A+", "A", "B", "C", "F", "N/A"))),
              column(3, tags$p("Use these filters before opening the registry below.", style = "margin-top:30px; color:#64748b; font-weight:700;"))
            )
          )
        ),
        fluidRow(
          box(title = "Student Registry", width = 12, DTOutput("master_dt"))
        )
      ),
      tabItem(
        tabName = "admin_staff",
        fluidRow(
          box(
            title = "Pending Staff Approval Form",
            width = 6,
            uiOutput("staff_pending_selector"),
            uiOutput("staff_pending_profile"),
            uiOutput("staff_pending_actions")
          ),
          box(
            title = "Remove Staff Account",
            width = 6,
            uiOutput("staff_remove_selector"),
            uiOutput("staff_remove_profile"),
            uiOutput("staff_remove_actions")
          )
        ),
        fluidRow(
          box(
            title = "Approved Staff Directory",
            width = 12,
            uiOutput("staff_directory_ui")
          )
        ),
        fluidRow(
          box(title = "Staff Accounts", width = 12, DTOutput("staff_dt"))
        )
      ),
      tabItem(
        tabName = "admin_internal",
        fluidRow(
          box(
            title = "Internal Examination Module",
            width = 12,
            fluidRow(
              column(3, selectInput("internal_reg", "Student", choices = student_choices)),
              column(3, uiOutput("internal_semester_ui")),
              column(3, textInput("internal_name", "Student Name")),
              column(3, textInput("internal_dept", "Department"))
            ),
            tags$p("Internal score is auto-built as Assignment + Attendance + Internal Exams. For 100-mark papers it becomes 5 + 5 + 10 = 20. For 50-mark papers it becomes 2.5 + 2.5 + 5 = 10.", style = "font-weight:700; color:#64748b;"),
            uiOutput("internal_inputs_ui"),
            tags$div(
              style = "margin-top:18px; padding:18px; border-radius:18px; background:#f8fafc;",
              uiOutput("internal_calc_box"),
              br(),
              actionButton("save_internal", "Save Internal Examination", class = "btn-uibs")
            )
          )
        )
      ),
      tabItem(
        tabName = "admin_ops",
        fluidRow(
          box(
            title = "Attendance Module",
            width = 7,
            selectInput("ops_reg", "Student", choices = student_choices),
            uiOutput("ops_semester_ui"),
            uiOutput("ops_attendance_ui"),
            textInput("ops_mentor", "Mentor"),
            actionButton("save_ops", "Update Operations Record", class = "btn-uibs")
          ),
          box(
            title = "Academic Excellence List",
            width = 5,
            DTOutput("topper_dt")
          )
        ),
        fluidRow(
          box(title = "Attendance vs Result", width = 6, plotOutput("g_attendance_vs_result", height = 300)),
          box(title = "Fee Compliance Snapshot", width = 6, plotOutput("g_fee_status", height = 300))
        )
      ),
      tabItem(
        tabName = "admin_fees",
        fluidRow(
          box(
            title = "Fees Management Module",
            width = 8,
            fluidRow(
              column(3, selectInput("fee_reg", "Student", choices = student_choices)),
              column(3, uiOutput("fee_semester_ui")),
              column(3, textInput("fee_name", "Student Name")),
              column(3, textInput("fee_dept", "Department"))
            ),
            fluidRow(
              column(4, selectInput("fee_status", "Fee Status", choices = FEE_OPTIONS)),
              column(4, numericInput("fee_total_amount", "Total Fee Amount", value = 0, min = 0, step = 100)),
              column(4, numericInput("fee_paid_amount", "Amount Paid", value = 0, min = 0, step = 100))
            ),
            fluidRow(
              column(4, numericInput("fee_scholarship_amount", "Scholarship / Concession", value = 0, min = 0, step = 100)),
              column(4, textInput("fee_last_payment_date", "Last Payment Date")),
              column(4, textInput("fee_receipt_no", "Receipt Number"))
            ),
            textAreaInput("fee_remarks", "Fee Remarks", rows = 4),
            tags$div(
              style = "margin-top:18px; padding:18px; border-radius:18px; background:#f8fafc;",
              uiOutput("fee_summary_ui"),
              br(),
              actionButton("save_fee", "Update Fee Record", class = "btn-uibs")
            )
          ),
          box(
            title = "Fee Overview",
            width = 4,
            uiOutput("fee_profile_ui")
          )
        )
      ),
      tabItem(
        tabName = "admin_notices",
        fluidRow(
          box(
            title = "Publish Campus Announcement",
            width = 5,
            textInput("notice_title", "Title"),
            textAreaInput("notice_body", "Message", rows = 6),
            selectInput("notice_audience", "Audience", choices = c("All", "Students", "Staff")),
            actionButton("save_notice", "Publish Announcement", class = "btn-uibs")
          ),
          box(
            title = "Recent Announcements",
            width = 7,
            uiOutput("notice_feed_admin")
          )
        )
      ),
      tabItem(
        tabName = "staff_dash",
        div(
          class = "hero-panel",
          div(class = "hero-title", "Staff Command Desk"),
          p(class = "hero-sub", "Work with classes, internal assessments, timetable and academic operations from one place.")
        ),
        fluidRow(
          box(title = "My Staff Profile", width = 4, uiOutput("staff_profile_preview")),
          box(title = "Today Attendance Snapshot", width = 8, uiOutput("staff_attendance_summary"))
        )
      ),
      tabItem(
        tabName = "staff_profile",
        fluidRow(
          box(title = "Staff Profile", width = 12, uiOutput("staff_profile_full"))
        )
      ),
      tabItem(
        tabName = "staff_daily_attendance",
        fluidRow(
          box(
            title = "Subject Attendance Sheet",
            width = 12,
            uiOutput("daily_access_note"),
            fluidRow(
              column(3, textInput("daily_marker_name", "Marked By")),
              column(3, selectInput("daily_dept", "Department", choices = c("SELECT" = "", DEPARTMENTS))),
              column(3, uiOutput("daily_semester_ui")),
              column(3, textInput("daily_date", "Attendance Date", value = as.character(Sys.Date())))
            ),
            fluidRow(
              column(6, uiOutput("daily_subject_ui")),
              column(6, textInput("daily_class_time", "Class Time", value = format(Sys.time(), "%H:%M")))
            ),
            uiOutput("daily_cr_assignment_ui"),
            tags$p("Attendance is recorded subject-wise for each class time. Faculty, HoD and Principal can assign one CR for the selected class, and only that approved CR can access the attendance sheet.", style = "color:#64748b; font-weight:700;"),
            uiOutput("daily_attendance_cards"),
            br(),
            actionButton("save_daily_attendance", "Save Daily Attendance", class = "btn-uibs")
          )
        )
      ),
      tabItem(
        tabName = "shared_timetable",
        fluidRow(
          box(
            title = "Semester Time Table",
            width = 12,
            fluidRow(
              column(4, selectInput("tt_dept", "Department", choices = c("SELECT" = "", DEPARTMENTS))),
              column(4, selectInput("tt_semester", "Semester", choices = c("SELECT" = "", SEMESTER_OPTIONS))),
              column(
                4,
                tags$div(
                  style = "padding-top:28px; color:#64748b; font-weight:700;",
                  "The timetable stays blank until HoD, Principal or Admin fills the format and saves it."
                )
              )
            ),
            uiOutput("timetable_editor_ui"),
            tags$hr(),
            DTOutput("timetable_dt")
          )
        )
      ),
      tabItem(
        tabName = "shared_highlights",
        fluidRow(
          box(title = "College Highlights", width = 12, uiOutput("college_highlights_ui"))
        )
      ),
      tabItem(
        tabName = "student_dash",
        div(
          class = "hero-panel",
          div(class = "hero-title", "Student Success Hub"),
          p(class = "hero-sub", "Your academic performance, attendance position and essential campus status in one view.")
        ),
        fluidRow(
          box(title = "Semester Record", width = 12, uiOutput("student_semester_ui"))
        ),
        fluidRow(
          column(4, box(width = NULL, uiOutput("stu_profile"))),
          column(8, box(width = NULL, title = "Subject Benchmark", plotOutput("stu_benchmark", height = 420)))
        ),
        fluidRow(
          box(title = "Academic Trend", width = 6, plotOutput("stu_progress_plot", height = 280)),
          box(title = "Eligibility Snapshot", width = 6, uiOutput("student_service_cards"))
        )
      ),
      tabItem(
        tabName = "student_marks",
        fluidRow(
          box(title = "Official Marksheet", width = 7, DTOutput("stu_marks_dt")),
          box(title = "Performance Summary", width = 5, uiOutput("student_summary_panel"))
        )
      ),
      tabItem(
        tabName = "student_services",
        fluidRow(
          box(
            title = "Verified Document Downloads",
            width = 6,
            p("Download institution-formatted documents for records and administrative use."),
            downloadButton("dl_transcript", "Official Transcript (A4)", class = "btn-uibs btn-block"),
            br(),
            downloadButton("dl_id", "Student ID Card (CR80)", class = "btn-uibs btn-block")
          ),
          box(
            title = "Student Services Status",
            width = 6,
            uiOutput("student_services_status")
          )
        )
      ),
      tabItem(
        tabName = "student_notices",
        fluidRow(box(title = "Campus Announcements", width = 12, uiOutput("notice_feed_student")))
      ),
      tabItem(tabName = "logout", div())
      )
    )
  })

  output$student_semester_ui <- renderUI({
    req(user_reg())
    records <- student_semester_records(get_students(), user_reg())
    semesters <- unique(records$Semester)
    semesters <- semesters[nzchar(semesters)]
    if (length(semesters) == 0) {
      return(tags$p("No semester records available yet.", style = "color:#64748b; font-weight:700;"))
    }
    selectInput(
      "student_semester",
      "View Semester",
      choices = semesters,
      selected = if (!is.null(selected_student_semester()) && selected_student_semester() %in% semesters) selected_student_semester() else semesters[1]
    )
  })

  observeEvent(input$student_semester, {
    if (!is.null(input$student_semester) && nzchar(input$student_semester)) {
      selected_student_semester(input$student_semester)
    }
  }, ignoreInit = TRUE)

  internal_student <- reactive({
    if (is.null(input$internal_reg) || !nzchar(input$internal_reg)) return(NULL)
    semester <- if (!is.null(input$internal_semester) && nzchar(input$internal_semester)) input$internal_semester else ""
    db <- get_students()
    if (nzchar(semester)) {
      rows <- db[db$RegNo == input$internal_reg & db$Semester == semester, , drop = FALSE]
      if (nrow(rows) > 0) return(rows[1, , drop = FALSE])
    }
    latest_student_record(db, input$internal_reg)
  })

  output$internal_semester_ui <- renderUI({
    if (is.null(input$internal_reg) || !nzchar(input$internal_reg)) {
      return(selectInput("internal_semester", "Semester", choices = c("SELECT" = "")))
    }
    latest <- latest_student_record(get_students(), input$internal_reg)
    semesters <- get_semester_choices(latest$Dept[1])
    selected <- if (!is.null(input$internal_semester) && input$internal_semester %in% semesters) {
      input$internal_semester
    } else if (nrow(latest) > 0 && latest$Semester[1] %in% semesters) {
      latest$Semester[1]
    } else {
      ""
    }
    selectInput("internal_semester", "Semester", choices = c("SELECT" = "", semesters), selected = selected)
  })

  output$ops_semester_ui <- renderUI({
    if (is.null(input$ops_reg) || !nzchar(input$ops_reg)) {
      return(selectInput("ops_semester", "Semester", choices = c("SELECT" = "")))
    }
    latest <- latest_student_record(get_students(), input$ops_reg)
    semesters <- get_semester_choices(latest$Dept[1])
    selected <- if (!is.null(input$ops_semester) && input$ops_semester %in% semesters) {
      input$ops_semester
    } else if (nrow(latest) > 0 && latest$Semester[1] %in% semesters) {
      latest$Semester[1]
    } else {
      ""
    }
    selectInput("ops_semester", "Semester", choices = c("SELECT" = "", semesters), selected = selected)
  })

  fee_student <- reactive({
    if (is.null(input$fee_reg) || !nzchar(input$fee_reg)) return(NULL)
    semester <- if (!is.null(input$fee_semester) && nzchar(input$fee_semester)) input$fee_semester else ""
    db <- get_students()
    if (nzchar(semester)) {
      rows <- db[db$RegNo == input$fee_reg & db$Semester == semester, , drop = FALSE]
      if (nrow(rows) > 0) return(rows[1, , drop = FALSE])
    }
    latest_student_record(db, input$fee_reg)
  })

  output$fee_semester_ui <- renderUI({
    if (is.null(input$fee_reg) || !nzchar(input$fee_reg)) {
      return(selectInput("fee_semester", "Semester", choices = c("SELECT" = "")))
    }
    latest <- latest_student_record(get_students(), input$fee_reg)
    semesters <- get_semester_choices(latest$Dept[1])
    selected <- if (!is.null(input$fee_semester) && input$fee_semester %in% semesters) {
      input$fee_semester
    } else if (nrow(latest) > 0 && latest$Semester[1] %in% semesters) {
      latest$Semester[1]
    } else {
      ""
    }
    selectInput("fee_semester", "Semester", choices = c("SELECT" = "", semesters), selected = selected)
  })

  observeEvent(internal_student(), {
    stu <- internal_student()
    if (is.null(stu) || nrow(stu) == 0) return()
    updateTextInput(session, "internal_name", value = stu$Name[1])
    updateTextInput(session, "internal_dept", value = stu$Dept[1])
  })

  output$internal_inputs_ui <- renderUI({
    stu <- internal_student()
    if (is.null(stu) || nrow(stu) == 0) {
      return(tags$p("Select a student and semester to enter internal marks.", style = "color:#64748b; font-weight:700;"))
    }
    subject_info <- get_subject_info(stu$Dept[1], ifelse(nzchar(input$internal_semester), input$internal_semester, resolve_student_semester(stu)))
    if (nrow(subject_info) == 0) return(NULL)

    tagList(lapply(seq_len(nrow(subject_info)), function(i) {
      cap_info <- internal_component_caps(subject_info$max_mark[i])
      exam_max <- internal_exam_raw_max(subject_info$max_mark[i])
      assignment_cap <- cap_info$AssignmentCap[1]
      attendance_cap <- cap_info$AttendanceCap[1]
      fluidRow(
        column(4, tags$div(style = "font-weight:700; padding-top:28px;", paste(subject_info$paper_code[i], "-", subject_info$subject[i]))),
        column(2, numericInput(paste0("internal1_", subject_info$code[i]), paste0("Internal 1 (Max ", exam_max, ")"), value = stu[[internal1_col(i)]][1], min = 0, max = exam_max)),
        column(2, numericInput(paste0("internal2_", subject_info$code[i]), paste0("Internal 2 (Max ", exam_max, ")"), value = stu[[internal2_col(i)]][1], min = 0, max = exam_max)),
        column(2, numericInput(paste0("assignment_", subject_info$code[i]), paste0("Assignment (Max ", assignment_cap, ")"), value = stu[[assignment_col(i)]][1], min = 0, max = assignment_cap)),
        column(1, tags$div(style = "font-weight:700; padding-top:28px;", paste0("Attendance (Max ", attendance_cap, ")")), tags$div(style = "font-weight:800; color:#64748b;", textOutput(paste0("attendance_bonus_", subject_info$code[i]), inline = TRUE))),
        column(1, tags$div(style = "font-weight:700; padding-top:28px;", "Total"), tags$div(style = "font-weight:800; color:#64748b;", textOutput(paste0("internal_total_", subject_info$code[i]), inline = TRUE))),
        column(12, tags$p(paste0("Attendance bonus uses semester attendance automatically (Max ", attendance_cap, ")."), style = "margin:-2px 0 8px 0; color:#94a3b8; font-weight:700;"))
      )
    }))
  })

  output$internal_calc_box <- renderUI({
    stu <- internal_student()
    if (is.null(stu) || nrow(stu) == 0) return(NULL)
    subject_info <- get_subject_info(stu$Dept[1], ifelse(nzchar(input$internal_semester), input$internal_semester, resolve_student_semester(stu)))
    if (nrow(subject_info) == 0) return(NULL)
    totals <- vapply(seq_len(nrow(subject_info)), function(i) {
      working <- stu
      working[[internal1_col(i)]][1] <- ifelse(is.null(input[[paste0("internal1_", subject_info$code[i])]]), working[[internal1_col(i)]][1], input[[paste0("internal1_", subject_info$code[i])]])
      working[[internal2_col(i)]][1] <- ifelse(is.null(input[[paste0("internal2_", subject_info$code[i])]]), working[[internal2_col(i)]][1], input[[paste0("internal2_", subject_info$code[i])]])
      working[[assignment_col(i)]][1] <- ifelse(is.null(input[[paste0("assignment_", subject_info$code[i])]]), working[[assignment_col(i)]][1], input[[paste0("assignment_", subject_info$code[i])]])
      student_internal_details(working, subject_info)$InternalScore[i]
    }, numeric(1))
    tagList(
      tags$p("Internal marks are auto-calculated as Assignment + Attendance Bonus + Internal Exam component, and will be added to the semester result automatically.", style = "font-weight:700; color:#64748b;"),
      tags$p(paste("Internal Total:", round_whole(sum(totals))), style = "font-weight:800; color:#5b1f41;")
    )
  })

  output$grade_inputs_ui <- renderUI({
      subject_info <- selected_subject_info()
      if (is.null(subject_info) || nrow(subject_info) == 0) {
        return(
          div(
            style = "padding:24px 0; color:#64748b; font-weight:700;",
            "Select department and semester, or load a student, to view subjects."
          )
        )
      }
      split_idx <- ceiling(nrow(subject_info) / 2)
      right_idx <- if (split_idx < nrow(subject_info)) seq(split_idx + 1, nrow(subject_info)) else integer(0)

    fluidRow(
      column(
        6,
        lapply(seq_len(split_idx), function(i) {
          numericInput(
            inputId = paste0("grade_", subject_info$code[i]),
            label = paste0(subject_info$paper_code[i], " - ", subject_info$subject[i], " (External Max ", external_max_mark(subject_info$max_mark[i]), ")"),
            value = NA,
            min = 0,
            max = external_max_mark(subject_info$max_mark[i])
          )
        })
      ),
      column(
        6,
        lapply(right_idx, function(i) {
          numericInput(
            inputId = paste0("grade_", subject_info$code[i]),
            label = paste0(subject_info$paper_code[i], " - ", subject_info$subject[i], " (External Max ", external_max_mark(subject_info$max_mark[i]), ")"),
            value = NA,
            min = 0,
            max = external_max_mark(subject_info$max_mark[i])
          )
        })
      )
    )
  })

  observeEvent(input$do_login, {
    req(input$l_role)

    if (identical(input$l_role, "Admin")) {
      if (identical(input$l_user, "CSB") && identical(input$l_pass, "CSB@123")) {
        logged_in(TRUE)
        user_role("admin")
        user_reg("CSB")
        current_staff_designation("Admin")
        go_to_default_tab("admin")
      } else {
        showNotification("Admin access denied.", type = "error")
      }
      return()
    }

    users <- get_users()
    expected_role <- tolower(input$l_role)
    match <- users[users$RegNo == input$l_user & users$Password == input$l_pass & users$Role == expected_role, , drop = FALSE]

    if (nrow(match) > 0) {
      if (identical(expected_role, "faculty") && !identical(match$Approved[1], "Approved")) {
        showNotification("Staff registration is still pending admin approval.", type = "error")
        return()
      }
      logged_in(TRUE)
      user_role(expected_role)
      user_reg(input$l_user)
      current_staff_designation(match$StaffRole[1])
      go_to_default_tab(expected_role)
    } else {
      showNotification("Invalid ID or password.", type = "error")
    }
  })

  observeEvent(input$show_register, {
    auth_result_title("")
    auth_result_body("")
    auth_mode("register")
  })

  observeEvent(input$show_login, {
    auth_result_title("")
    auth_result_body("")
    auth_mode("login")
  })

  observeEvent(input$show_forgot, {
    auth_result_title("")
    auth_result_body("")
    auth_mode("forgot")
  })

  output$auth_panel_ui <- renderUI({
    if (identical(auth_mode(), "login")) {
      return(
        tagList(
          div(
            class = "auth-section",
            div(class = "auth-section-title", "Sign In Details"),
            div(
              class = "auth-grid",
              div(class = "auth-span-2", textInput("l_user", "Registration ID / Staff ID")),
              passwordInput("l_pass", "Password"),
              selectInput("l_role", "Account Type", choices = c("SELECT" = "", "Student", "Faculty", "Admin"))
            )
          ),
          div(
            class = "auth-actions",
            actionButton("do_login", "Access Dashboard", class = "btn-login"),
            span(class = "auth-note", "Use the approved role and password linked to this registration ID.")
          )
        )
      )
    }

    if (identical(auth_mode(), "register")) {
      return(
        tagList(
          div(
            class = "auth-section",
            div(class = "auth-section-title", "Basic Details"),
            div(
              class = "auth-grid",
              selectInput("r_role", "Register As", choices = c("SELECT" = "", "Student", "Faculty")),
              textInput("r_no", "Registration ID"),
              textInput("r_name", "Full Name"),
              passwordInput("r_pass", "Create Password")
            )
          ),
          uiOutput("register_role_fields"),
          div(
            class = "auth-actions",
            actionButton("do_register", "Create Account", class = "btn-login"),
            span(class = "auth-note", "Fields marked by the system rules are compulsory before account creation.")
          )
        )
      )
    }

    if (identical(auth_mode(), "register_done")) {
      return(
        tagList(
          div(
            class = "auth-section",
            div(class = "auth-section-title", "Registration Status"),
            tags$div(style = "font-size:22px; font-weight:900; margin-bottom:8px;", auth_result_title()),
            tags$p(style = "color:rgba(255,255,255,0.84); margin-bottom:0;", auth_result_body())
          ),
          div(
            class = "auth-actions",
            tags$span(class = "auth-note", "Use the links below to return to login or open a fresh registration form.")
          )
        )
      )
    }

    tagList(
      div(
        class = "auth-section",
        div(class = "auth-section-title", "Password Reset"),
        div(
          class = "auth-grid",
          selectInput("fp_role", "Account Type", choices = c("SELECT" = "", "Student", "Faculty")),
          textInput("fp_user", "Registration ID / Faculty ID"),
          passwordInput("fp_new_pass", "New Password"),
          passwordInput("fp_confirm_pass", "Confirm Password")
        )
      ),
      div(
        class = "auth-actions",
        actionButton("reset_pass", "Update Password", class = "btn-login"),
        span(class = "auth-note", "Admin password remains the fixed institutional credential.")
      )
    )
  })

  output$register_role_fields <- renderUI({
    if (identical(input$r_role, "Student")) {
      return(tagList(
        div(
          class = "auth-section",
          div(class = "auth-section-title", "Academic Information"),
          div(
            class = "auth-grid",
            selectInput("r_dept", "Department", choices = c("SELECT" = "", DEPARTMENTS)),
            selectInput("r_year", "Academic Year", choices = c("SELECT" = "", YEARS)),
            div(class = "auth-span-2", textInput("r_mentor", "Mentor Name")),
            div(class = "auth-span-2", fileInput("r_photo", "Profile Picture", accept = c("image/png", "image/jpeg")))
          )
        ),
        div(
          class = "auth-section",
          div(class = "auth-section-title", "Address Details"),
          div(
            class = "auth-grid",
            div(class = "auth-span-2", textInput("r_address1", "House / Street")),
            div(class = "auth-span-2", textInput("r_address2", "Area / Landmark"))
          ),
          div(
            class = "auth-grid auth-grid-3",
            textInput("r_city", "City"),
            textInput("r_state", "State"),
            textInput("r_pincode", "Pincode")
          )
        )
      ))
    }

    if (identical(input$r_role, "Faculty")) {
      return(tagList(
        div(
          class = "auth-section",
          div(class = "auth-section-title", "Staff Verification"),
          div(
            class = "auth-grid",
            div(class = "auth-span-2", textInput("r_email", "College Email ID")),
            selectInput("r_staff_role", "Staff Role", choices = c("SELECT" = "", STAFF_ROLE_OPTIONS)),
            div(class = "auth-span-2", fileInput("r_photo", "Staff Photo", accept = c("image/png", "image/jpeg")))
          )
        ),
        div(
          class = "auth-section",
          div(class = "auth-section-title", "Address Details"),
          div(
            class = "auth-grid",
            div(class = "auth-span-2", textInput("r_address1", "House / Street")),
            div(class = "auth-span-2", textInput("r_address2", "Area / Landmark"))
          ),
          div(
            class = "auth-grid auth-grid-3",
            textInput("r_city", "City"),
            textInput("r_state", "State"),
            textInput("r_pincode", "Pincode")
          )
        )
      ))
    }

    NULL
  })

  observeEvent(input$reset_pass, {
    if (!nzchar(input$fp_role) || !nzchar(input$fp_user) || !nzchar(input$fp_new_pass) || !nzchar(input$fp_confirm_pass)) {
      showNotification("Complete all password reset fields.", type = "error")
      return()
    }
    if (!identical(input$fp_new_pass, input$fp_confirm_pass)) {
      showNotification("New password and confirmation do not match.", type = "error")
      return()
    }

    users <- get_users()
    idx <- which(users$RegNo == input$fp_user & users$Role == tolower(input$fp_role))
    if (length(idx) == 0) {
      showNotification("Account not found for password reset.", type = "error")
      return()
    }

    users[idx, "Password"] <- input$fp_new_pass
    if (!write_users_db(users)) return()
    auth_mode("login")
    showNotification("Password updated successfully.", type = "message")
  })

  observeEvent(input$do_register, {
    registering_student <- identical(input$r_role, "Student")
    registering_staff <- identical(input$r_role, "Faculty")
    if (!nzchar(input$r_role) || !nzchar(input$r_no) || !nzchar(input$r_name) || !nzchar(input$r_pass) ||
        (registering_student && (!nzchar(input$r_dept) || !nzchar(input$r_year))) ||
        (registering_staff && (!nzchar(input$r_email) || !nzchar(input$r_staff_role)))) {
      showNotification("Complete the registration form before submitting.", type = "error")
      return()
    }

    users <- get_users()
    students <- ensure_schema(read_safe_csv(STUDENTS_DB, students_template), students_template)

    reg_no <- toupper(trimws(input$r_no))
    if (!is_valid_uibs_regno(reg_no)) {
      showNotification("Registration ID must start with U19TO.", type = "error")
      return()
    }

    if (registering_staff && !is_valid_college_email(input$r_email)) {
      showNotification("Staff email must end with @uibsblr.com.", type = "error")
      return()
    }

    if (is.null(input$r_photo)) {
      showNotification("Profile photo is compulsory.", type = "error")
      return()
    }

    if (!nzchar(input$r_address1) || !nzchar(input$r_city) || !nzchar(input$r_state) || !nzchar(input$r_pincode)) {
      showNotification("Complete the address section before registering.", type = "error")
      return()
    }

    if (reg_no %in% users$RegNo) {
      showNotification("Registration ID already exists.", type = "error")
      return()
    }

    photo_rel <- ""
    if (!is.null(input$r_photo)) {
      ext <- tolower(tools::file_ext(input$r_photo$name))
      if (ext %in% c("png", "jpg", "jpeg")) {
        file_name <- paste0(gsub("[^A-Za-z0-9_-]", "", reg_no), ".", ext)
        file.copy(input$r_photo$datapath, file.path(PHOTO_DIR, file_name), overwrite = TRUE)
        photo_rel <- file.path("photos", file_name)
      }
    }

    users <- rbind(
      users,
      data.frame(
        RegNo = reg_no,
        Name = input$r_name,
        Email = if (registering_staff) input$r_email else "",
        Password = input$r_pass,
        Role = tolower(input$r_role),
        StaffRole = if (registering_staff) input$r_staff_role else "",
        Approved = if (registering_staff) "Pending" else "Approved",
        Photo = photo_rel,
        Dept = if (registering_student) input$r_dept else "",
        Year = if (registering_student) input$r_year else "",
        CRAccess = "No",
        CRDept = "",
        CRYear = "",
        CRSemester = "",
        CRAssignedBy = "",
        CRAssignedByRole = "",
        CRAssignedOn = "",
        AddressLine1 = input$r_address1,
        AddressLine2 = input$r_address2,
        City = input$r_city,
        State = input$r_state,
        Pincode = input$r_pincode,
        UpdatedAt = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )
    )

    if (registering_student) {
      subject_info <- get_subject_info(input$r_dept, "Semester 1")
      new_student <- students_template[0, ]
      new_student[1, ] <- students_template[1, ]
      new_student$RegNo[1] <- reg_no
      new_student$Name[1] <- input$r_name
      new_student$Dept[1] <- input$r_dept
      new_student$Year[1] <- input$r_year
      new_student$Semester[1] <- "Semester 1"
      new_student$Scheme[1] <- unique(subject_info$scheme)[1]
      new_student$Photo[1] <- photo_rel
      new_student$Lang1[1] <- if (requires_language(input$r_dept, "Semester 1")) "Additional English" else ""
      new_student$TotalCredits[1] <- get_total_credits(subject_info)
      new_student$Grade[1] <- "N/A"
      new_student$FeeStatus[1] <- "Pending"
      new_student$Mentor[1] <- input$r_mentor
      new_student$AddressLine1[1] <- input$r_address1
      new_student$AddressLine2[1] <- input$r_address2
      new_student$City[1] <- input$r_city
      new_student$State[1] <- input$r_state
      new_student$Pincode[1] <- input$r_pincode
      new_student$UpdatedAt[1] <- as.character(Sys.time())

      students <- rbind(students, new_student)
    }

    if (!write_users_db(users)) return()
    if (registering_student && !write_students_db(students)) return()

    logged_in(FALSE)
    user_role(NULL)
    user_reg(NULL)
    current_staff_designation("")
    if (registering_student) {
      auth_result_title("Student Registration Completed")
      auth_result_body("Your student account has been created successfully. Use your registration ID and password to sign in from the login page.")
      showNotification("Student account created successfully.", type = "message")
    } else {
      auth_result_title("Staff Registration Submitted")
      auth_result_body("Your staff account has been saved and is now waiting for admin approval. You will be able to log in only after approval.")
      showNotification("Staff registration submitted. Admin approval is required before login.", type = "message")
    }
    auth_mode("register_done")
  })

  observeEvent(input$tabs, {
    if (identical(input$tabs, "logout")) {
      logged_in(FALSE)
      user_role(NULL)
      user_reg(NULL)
      current_staff_designation("")
      active_tab("admin_dash")
      auth_result_title("")
      auth_result_body("")
      auth_mode("login")
    }
  })

  observeEvent(input$search_btn, {
    stu <- latest_student_record(get_students(), input$res_id)
    if (nrow(stu) == 0) {
      showNotification("Student not found in registry.", type = "warning")
      return()
    }

    updateTextInput(session, "res_name", value = stu$Name[1])
    updateSelectInput(session, "res_dept", selected = stu$Dept[1])
    updateSelectInput(session, "res_year", selected = stu$Year[1])
    updateSelectInput(session, "res_semester", selected = ifelse(nzchar(stu$Semester[1]), stu$Semester[1], "Semester 1"))
    updateSelectInput(session, "res_lang", selected = if (requires_language(stu$Dept[1], ifelse(nzchar(stu$Semester[1]), stu$Semester[1], "Semester 1"))) stu$Lang1[1] else "")
    updateNumericInput(session, "grade_prev", value = stu$PrevCGPA[1])
    updateTextInput(session, "grade_mentor", value = stu$Mentor[1])

    subject_info <- get_subject_info(stu$Dept[1], ifelse(nzchar(stu$Semester[1]), stu$Semester[1], "Semester 1"))
    for (i in seq_len(nrow(subject_info))) {
      updateNumericInput(session, paste0("grade_", subject_info$code[i]), value = stu[[subject_info$code[i]]][1])
    }
  })

  observeEvent(list(input$res_id, input$res_semester), {
    if (is.null(input$res_id) || !nzchar(input$res_id) || is.null(input$res_semester) || !nzchar(input$res_semester)) return()
    db <- get_students()
    stu <- db[db$RegNo == input$res_id & db$Semester == input$res_semester, , drop = FALSE]
    if (nrow(stu) == 0) return()
    stu <- student_semester_records(stu, input$res_id)[1, , drop = FALSE]
    updateTextInput(session, "res_name", value = stu$Name[1])
    updateSelectInput(session, "res_dept", selected = stu$Dept[1])
    updateSelectInput(session, "res_year", selected = stu$Year[1])
    updateSelectInput(session, "res_lang", selected = if (requires_language(stu$Dept[1], input$res_semester)) stu$Lang1[1] else "")
    updateNumericInput(session, "grade_prev", value = stu$PrevCGPA[1])
    updateTextInput(session, "grade_mentor", value = stu$Mentor[1])

    subject_info <- get_subject_info(stu$Dept[1], input$res_semester)
    for (i in seq_len(nrow(subject_info))) {
      updateNumericInput(session, paste0("grade_", subject_info$code[i]), value = stu[[subject_info$code[i]]][1])
    }
  }, ignoreInit = TRUE)

  observe({
    stu <- internal_student()
    if (is.null(stu) || nrow(stu) == 0) return()
    subject_info <- get_subject_info(stu$Dept[1], ifelse(nzchar(input$internal_semester), input$internal_semester, resolve_student_semester(stu)))
    lapply(seq_len(nrow(subject_info)), function(i) {
      local({
        idx_local <- i
        code_local <- subject_info$code[idx_local]
        output[[paste0("attendance_bonus_", code_local)]] <- renderText({
          working <- stu
          working[[internal1_col(idx_local)]][1] <- ifelse(is.null(input[[paste0("internal1_", code_local)]]), working[[internal1_col(idx_local)]][1], input[[paste0("internal1_", code_local)]])
          working[[internal2_col(idx_local)]][1] <- ifelse(is.null(input[[paste0("internal2_", code_local)]]), working[[internal2_col(idx_local)]][1], input[[paste0("internal2_", code_local)]])
          working[[assignment_col(idx_local)]][1] <- ifelse(is.null(input[[paste0("assignment_", code_local)]]), working[[assignment_col(idx_local)]][1], input[[paste0("assignment_", code_local)]])
          details <- student_internal_details(working, subject_info)
          cap_info <- internal_component_caps(subject_info$max_mark[idx_local])
          paste0(details$AttendanceBonus[idx_local], " / ", cap_info$AttendanceCap[1])
        })
        output[[paste0("internal_total_", code_local)]] <- renderText({
          working <- stu
          working[[internal1_col(idx_local)]][1] <- ifelse(is.null(input[[paste0("internal1_", code_local)]]), working[[internal1_col(idx_local)]][1], input[[paste0("internal1_", code_local)]])
          working[[internal2_col(idx_local)]][1] <- ifelse(is.null(input[[paste0("internal2_", code_local)]]), working[[internal2_col(idx_local)]][1], input[[paste0("internal2_", code_local)]])
          working[[assignment_col(idx_local)]][1] <- ifelse(is.null(input[[paste0("assignment_", code_local)]]), working[[assignment_col(idx_local)]][1], input[[paste0("assignment_", code_local)]])
          paste0(student_internal_details(working, subject_info)$InternalScore[idx_local], " / ", internal_max_mark(subject_info$max_mark[idx_local]))
        })
      })
    })
  })

  observeEvent(input$save_internal, {
    if (is.null(input$internal_reg) || !nzchar(input$internal_reg) || is.null(input$internal_semester) || !nzchar(input$internal_semester)) {
      showNotification("Select student and semester before saving internals.", type = "error")
      return()
    }
    students <- ensure_schema(read_safe_csv(STUDENTS_DB, students_template), students_template)
    base_student <- latest_student_record(students, input$internal_reg)
    if (nrow(base_student) == 0) {
      showNotification("Student not found.", type = "error")
      return()
    }
    idx <- which(students$RegNo == input$internal_reg & students$Semester == input$internal_semester)
    if (length(idx) == 0) {
      students <- rbind(students, base_student[1, , drop = FALSE])
      idx <- nrow(students)
      students[idx, "Semester"] <- input$internal_semester
      students[idx, paste0("M", seq_len(MAX_SUBJECT_SLOTS))] <- 0
    } else {
      idx <- idx[1]
    }
    subject_info <- get_subject_info(base_student$Dept[1], input$internal_semester)
    for (i in seq_len(nrow(subject_info))) {
      students[idx, internal1_col(i)] <- ifelse(is.null(input[[paste0("internal1_", subject_info$code[i])]]), 0, input[[paste0("internal1_", subject_info$code[i])]])
      students[idx, internal2_col(i)] <- ifelse(is.null(input[[paste0("internal2_", subject_info$code[i])]]), 0, input[[paste0("internal2_", subject_info$code[i])]])
      students[idx, assignment_col(i)] <- ifelse(is.null(input[[paste0("assignment_", subject_info$code[i])]]), 0, input[[paste0("assignment_", subject_info$code[i])]])
    }
    students[idx, "UpdatedAt"] <- as.character(Sys.time())
    if (!write_students_db(students)) return()
    showNotification("Internal examination record saved.", type = "message")
  })

  observeEvent(input$info_reg, {
    if (is.null(input$info_reg) || !nzchar(input$info_reg)) return()
    stu <- latest_student_record(get_students(), input$info_reg)
    if (nrow(stu) == 0) return()
    updateTextInput(session, "info_name", value = stu$Name[1])
    updateSelectInput(session, "info_dept", selected = stu$Dept[1])
    updateSelectInput(session, "info_year", selected = stu$Year[1])
    updateTextInput(session, "info_mentor", value = stu$Mentor[1])
    updateTextInput(session, "info_address1", value = stu$AddressLine1[1])
    updateTextInput(session, "info_address2", value = stu$AddressLine2[1])
    updateTextInput(session, "info_city", value = stu$City[1])
    updateTextInput(session, "info_state", value = stu$State[1])
    updateTextInput(session, "info_pincode", value = stu$Pincode[1])
  })

  observeEvent(input$save_student_info, {
    if (is.null(input$info_reg) || !nzchar(input$info_reg)) {
      showNotification("Select a student before updating info.", type = "error")
      return()
    }
    students <- ensure_schema(read_safe_csv(STUDENTS_DB, students_template), students_template)
    idx <- which(students$RegNo == input$info_reg)
    if (length(idx) == 0) {
      showNotification("Student not found.", type = "error")
      return()
    }
    photo_rel <- NULL
    if (!is.null(input$info_photo)) {
      ext <- tolower(tools::file_ext(input$info_photo$name))
      if (ext %in% c("png", "jpg", "jpeg")) {
        file_name <- paste0(gsub("[^A-Za-z0-9_-]", "", input$info_reg), ".", ext)
        file.copy(input$info_photo$datapath, file.path(PHOTO_DIR, file_name), overwrite = TRUE)
        photo_rel <- file.path("photos", file_name)
      }
    }
    students[idx, "Name"] <- input$info_name
    students[idx, "Dept"] <- input$info_dept
    students[idx, "Year"] <- input$info_year
    students[idx, "Mentor"] <- input$info_mentor
    students[idx, "AddressLine1"] <- input$info_address1
    students[idx, "AddressLine2"] <- input$info_address2
    students[idx, "City"] <- input$info_city
    students[idx, "State"] <- input$info_state
    students[idx, "Pincode"] <- input$info_pincode
    if (!is.null(photo_rel)) students[idx, "Photo"] <- photo_rel
    students[idx, "UpdatedAt"] <- as.character(Sys.time())
    if (!write_students_db(students)) return()
    showNotification("Student information updated.", type = "message")
  })

  pending_staff_records <- reactive({
    users <- get_users()
    rows <- users[tolower(trimws(users$Role)) == "faculty" & trimws(users$Approved) == "Pending", , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$UpdatedAt, decreasing = TRUE), , drop = FALSE]
  })

  output$staff_pending_selector <- renderUI({
    pending_staff <- pending_staff_records()
    if (nrow(pending_staff) == 0) {
      return(
        div(
          class = "pending-empty",
          "No pending staff requests are waiting for approval."
        )
      )
    }
    choice_labels <- paste(
      pending_staff$RegNo,
      ifelse(nzchar(pending_staff$Name), pending_staff$Name, pending_staff$RegNo),
      ifelse(nzchar(pending_staff$StaffRole), pending_staff$StaffRole, "Faculty"),
      sep = " | "
    )
    selected_pending <- if (!is.null(input$staff_manage_reg) && input$staff_manage_reg %in% pending_staff$RegNo) {
      input$staff_manage_reg
    } else {
      pending_staff$RegNo[1]
    }
    div(
      class = "pending-staff-picker",
      radioButtons(
        "staff_manage_reg",
        "Pending Staff",
        choices = stats::setNames(pending_staff$RegNo, choice_labels),
        selected = selected_pending
      )
    )
  })

  observe({
    pending_staff <- pending_staff_records()
    if (nrow(pending_staff) == 0) return()
    selected_reg <- if (is.null(input$staff_manage_reg)) "" else input$staff_manage_reg
    if (!selected_reg %in% pending_staff$RegNo) {
      updateRadioButtons(session, "staff_manage_reg", selected = pending_staff$RegNo[1])
    }
  })

  pending_staff_record <- reactive({
    pending_staff <- pending_staff_records()
    if (nrow(pending_staff) == 0) return(NULL)
    selected_reg <- if (!is.null(input$staff_manage_reg) && input$staff_manage_reg %in% pending_staff$RegNo) {
      input$staff_manage_reg
    } else {
      pending_staff$RegNo[1]
    }
    rows <- pending_staff[pending_staff$RegNo == selected_reg, , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    rows[1, , drop = FALSE]
  })

  approved_staff_records <- reactive({
    users <- get_users()
    rows <- users[tolower(trimws(users$Role)) == "faculty" & trimws(users$Approved) == "Approved", , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    updated_order <- suppressWarnings(as.POSIXct(rows$UpdatedAt, tz = "Asia/Calcutta"))
    updated_order[is.na(updated_order)] <- as.POSIXct("1970-01-01", tz = "Asia/Calcutta")
    rows[order(-as.numeric(updated_order)), , drop = FALSE]
  })

  render_staff_card <- function(staff_row, compact = FALSE) {
    if (is.null(staff_row) || nrow(staff_row) == 0) {
      return(tags$p("No staff profile selected.", style = "color:#64748b; font-weight:700;"))
    }
    staff_row <- ensure_schema(staff_row, users_template)
    staff_name <- ifelse(nzchar(staff_row$Name[1]), staff_row$Name[1], staff_row$RegNo[1])
    staff_role_label <- ifelse(nzchar(staff_row$StaffRole[1]), staff_row$StaffRole[1], "Faculty")
    approval_label <- ifelse(nzchar(staff_row$Approved[1]), staff_row$Approved[1], "Pending")
    photo <- safe_photo_path(staff_row$Photo[1])
    tagList(
      div(
        class = paste("profile-shell", if (compact) "profile-shell-compact" else ""),
        if (nzchar(photo)) {
          img(src = photo, class = if (compact) "profile-img profile-fallback-sm" else "profile-img")
        } else {
          span(class = paste("profile-fallback", if (compact) "profile-fallback-sm" else ""), substr(staff_name, 1, 1))
        },
        div(
          class = "profile-copy",
          h3(staff_name, style = "font-weight:900; margin:0 0 8px;"),
          p(paste(staff_row$RegNo[1], "|", staff_role_label, "|", approval_label), style = "color:#64748b; font-weight:700; margin:0 0 6px;"),
          p(ifelse(nzchar(staff_row$Email[1]), staff_row$Email[1], "Email not set"), style = "color:#64748b; font-weight:700; margin:0 0 6px;"),
          p(full_address(staff_row$AddressLine1[1], staff_row$AddressLine2[1], staff_row$City[1], staff_row$State[1], staff_row$Pincode[1]), style = "color:#475569; font-weight:700; margin:0;")
        )
      )
    )
  }

  output$staff_pending_profile <- renderUI({
    pending_staff <- pending_staff_records()
    if (nrow(pending_staff) == 0) {
      return(NULL)
    }
    render_staff_card(pending_staff_record())
  })

  output$staff_pending_actions <- renderUI({
    pending_staff <- pending_staff_records()
    if (nrow(pending_staff) == 0) {
      return(tags$p("Approve and reject actions appear here when a pending request is available.", style = "color:#64748b; font-weight:700; margin-top:8px;"))
    }
    div(
      class = "manager-form-actions",
      actionButton("approve_staff", "Approve Staff", class = "btn-uibs"),
      actionButton("reject_staff", "Reject Staff", class = "btn-uibs")
    )
  })

  output$staff_remove_selector <- renderUI({
    approved_staff <- approved_staff_records()
    if (nrow(approved_staff) == 0) {
      return(div(class = "pending-empty", "No approved staff accounts are available to remove."))
    }
    choice_labels <- paste(
      approved_staff$RegNo,
      ifelse(nzchar(approved_staff$Name), approved_staff$Name, approved_staff$RegNo),
      ifelse(nzchar(approved_staff$StaffRole), approved_staff$StaffRole, "Faculty"),
      sep = " | "
    )
    selected_staff <- if (!is.null(input$staff_remove_reg) && input$staff_remove_reg %in% approved_staff$RegNo) {
      input$staff_remove_reg
    } else {
      approved_staff$RegNo[1]
    }
    div(
      class = "pending-staff-picker",
      radioButtons(
        "staff_remove_reg",
        "Approved Staff",
        choices = stats::setNames(approved_staff$RegNo, choice_labels),
        selected = selected_staff
      )
    )
  })

  observe({
    approved_staff <- approved_staff_records()
    if (nrow(approved_staff) == 0) return()
    selected_reg <- if (is.null(input$staff_remove_reg)) "" else input$staff_remove_reg
    if (!selected_reg %in% approved_staff$RegNo) {
      updateRadioButtons(session, "staff_remove_reg", selected = approved_staff$RegNo[1])
    }
  })

  selected_approved_staff <- reactive({
    approved_staff <- approved_staff_records()
    if (nrow(approved_staff) == 0) return(NULL)
    selected_reg <- if (!is.null(input$staff_remove_reg) && input$staff_remove_reg %in% approved_staff$RegNo) {
      input$staff_remove_reg
    } else {
      approved_staff$RegNo[1]
    }
    rows <- approved_staff[approved_staff$RegNo == selected_reg, , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    rows[1, , drop = FALSE]
  })

  output$staff_remove_profile <- renderUI({
    approved_staff <- approved_staff_records()
    if (nrow(approved_staff) == 0) {
      return(NULL)
    }
    render_staff_card(selected_approved_staff())
  })

  output$staff_remove_actions <- renderUI({
    approved_staff <- approved_staff_records()
    if (nrow(approved_staff) == 0) {
      return(tags$p("Remove action appears here when an approved staff account is available.", style = "color:#64748b; font-weight:700; margin-top:8px;"))
    }
    div(
      class = "manager-form-actions",
      actionButton("remove_staff", "Remove Staff", class = "btn-uibs")
    )
  })

  output$staff_directory_ui <- renderUI({
    users <- get_users()
    approved_staff <- users[users$Role == "faculty" & users$Approved == "Approved", , drop = FALSE]
    if (nrow(approved_staff) == 0) {
      return(tags$p("No approved staff records yet.", style = "color:#64748b; font-weight:700;"))
    }
    tagList(lapply(seq_len(nrow(approved_staff)), function(i) {
      div(
        style = "display:flex; gap:16px; align-items:center; padding:14px 0; border-bottom:1px solid #e2e8f0;",
        render_staff_card(approved_staff[i, , drop = FALSE], compact = TRUE)
      )
    }))
  })

  output$staff_dt <- renderDT({
    users <- get_users()
    staff_db <- users[users$Role == "faculty", c("RegNo", "Name", "Email", "StaffRole", "Approved", "UpdatedAt"), drop = FALSE]
    if (nrow(staff_db) > 0) {
      staff_db$Name <- ifelse(nzchar(staff_db$Name), staff_db$Name, staff_db$RegNo)
      staff_db$StaffRole <- ifelse(nzchar(staff_db$StaffRole), staff_db$StaffRole, "Faculty")
      staff_db$Approved <- ifelse(nzchar(staff_db$Approved), staff_db$Approved, "Pending")
      updated_order <- suppressWarnings(as.POSIXct(staff_db$UpdatedAt, tz = "Asia/Calcutta"))
      updated_order[is.na(updated_order)] <- as.POSIXct("1970-01-01", tz = "Asia/Calcutta")
      staff_db <- staff_db[order(ifelse(staff_db$Approved == "Pending", 0, 1), -as.numeric(updated_order)), , drop = FALSE]
    }
    datatable(staff_db, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE, selection = "none")
  })

  observeEvent(input$approve_staff, {
    req(input$staff_manage_reg)
    users <- get_users()
    idx <- which(users$RegNo == input$staff_manage_reg & users$Role == "faculty")
    if (length(idx) == 0) return()
    users[idx, "Approved"] <- "Approved"
    users[idx, "UpdatedAt"] <- as.character(Sys.time())
    if (!write_users_db(users)) return()
    showNotification("Staff account approved.", type = "message")
  })

  observeEvent(input$reject_staff, {
    req(input$staff_manage_reg)
    users <- get_users()
    idx <- which(users$RegNo == input$staff_manage_reg & users$Role == "faculty" & users$Approved == "Pending")
    if (length(idx) == 0) return()
    users <- users[-idx[1], , drop = FALSE]
    if (!write_users_db(users)) return()
    showNotification("Pending staff registration removed.", type = "warning")
  })

  observeEvent(input$remove_staff, {
    req(input$staff_remove_reg)
    users <- get_users()
    idx <- which(users$RegNo == input$staff_remove_reg & users$Role == "faculty")
    if (length(idx) == 0) return()
    users <- users[-idx[1], , drop = FALSE]
    if (!write_users_db(users)) return()
    showNotification("Staff account removed.", type = "warning")
  })

  daily_attendance_access <- reactive({
    account <- current_user_account()
    is_cr <- has_cr_attendance_access(user_role(), account)
    marker_name <- if (nrow(account) > 0 && nzchar(account$Name[1])) account$Name[1] else if (!is.null(user_reg()) && nzchar(user_reg())) user_reg() else "Attendance Marker"
    marker_role <- if (identical(user_role(), "faculty")) {
      if (nzchar(current_staff_designation())) current_staff_designation() else "Faculty"
    } else if (is_cr) {
      "CR"
    } else if (is_admin_user(user_role())) {
      "Admin"
    } else {
      "User"
    }
    list(
      account = account,
      is_cr = is_cr,
      can_mark = can_manage_daily_attendance(user_role(), current_staff_designation(), account),
      can_assign = can_assign_cr_access(user_role(), current_staff_designation()),
      marker_name = marker_name,
      marker_role = marker_role,
      assigned_dept = if (is_cr && nrow(account) > 0) account$CRDept[1] else "",
      assigned_year = if (is_cr && nrow(account) > 0) account$CRYear[1] else "",
      assigned_semester = if (is_cr && nrow(account) > 0) account$CRSemester[1] else ""
    )
  })

  observe({
    access <- daily_attendance_access()
    if (!access$can_mark) return()
    updateTextInput(session, "daily_marker_name", value = access$marker_name)
    if (access$is_cr) {
      cr_dept_choices <- if (nzchar(access$assigned_dept)) stats::setNames(access$assigned_dept, access$assigned_dept) else c("SELECT" = "")
      updateSelectInput(
        session, "daily_dept",
        choices = cr_dept_choices,
        selected = if (nzchar(access$assigned_dept)) access$assigned_dept else ""
      )
    } else {
      current_dept <- if (!is.null(input$daily_dept) && input$daily_dept %in% DEPARTMENTS) input$daily_dept else ""
      updateSelectInput(session, "daily_dept", choices = c("SELECT" = "", DEPARTMENTS), selected = current_dept)
    }
  })

  output$staff_profile_preview <- renderUI({
    account <- current_user_account()
    if (nrow(account) == 0) {
      return(tags$p("Staff profile not available.", style = "color:#64748b; font-weight:700;"))
    }
    render_staff_card(account, compact = TRUE)
  })

  output$staff_profile_full <- renderUI({
    account <- current_user_account()
    if (nrow(account) == 0) {
      return(tags$p("Staff profile not available.", style = "color:#64748b; font-weight:700;"))
    }
    render_staff_card(account)
  })

  output$staff_attendance_summary <- renderUI({
    if (!identical(user_role(), "faculty")) {
      return(tags$p("Attendance summary is available on staff dashboards.", style = "color:#64748b; font-weight:700;"))
    }
    access <- daily_attendance_access()
    records <- get_daily_attendance()
    today_records <- records[records$MarkedByRegNo == user_reg() & records$Date == as.character(Sys.Date()), , drop = FALSE]
    present_count <- sum(today_records$Status == "Present", na.rm = TRUE)
    absent_count <- sum(today_records$Status == "Absent", na.rm = TRUE)
    session_count <- length(unique(paste(today_records$SubjectCode, today_records$ClassTime, sep = "@")))
    tagList(
      span(class = "info-chip", paste("Today:", as.character(Sys.Date()))),
      span(class = "info-chip", paste("Sessions:", session_count)),
      span(class = "info-chip", paste("Present:", present_count)),
      span(class = "info-chip", paste("Absent:", absent_count)),
      span(class = "info-chip", paste("Access:", access$marker_role))
    )
  })

  daily_attendance_scope <- reactive({
    access <- daily_attendance_access()
    list(
      dept = if (access$is_cr) access$assigned_dept else if (!is.null(input$daily_dept)) trimws(input$daily_dept) else "",
      semester = if (access$is_cr) access$assigned_semester else if (!is.null(input$daily_semester)) trimws(input$daily_semester) else "",
      year = if (access$is_cr) access$assigned_year else ""
    )
  })

  daily_subjects <- reactive({
    scope <- daily_attendance_scope()
    if (!nzchar(scope$dept) || !nzchar(scope$semester)) {
      return(default_subject_info[0, , drop = FALSE])
    }
    get_subject_info(scope$dept, scope$semester)
  })

  daily_attendance_students <- reactive({
    scope <- daily_attendance_scope()
    if (!nzchar(scope$dept) || !nzchar(scope$semester)) {
      return(get_students()[0, , drop = FALSE])
    }
    db <- get_students()
    rows <- db[db$Dept == scope$dept & db$Semester == scope$semester, , drop = FALSE]
    if (nzchar(scope$year)) rows <- rows[rows$Year == scope$year, , drop = FALSE]
    rows[order(rows$Name), , drop = FALSE]
  })

  current_class_cr <- reactive({
    scope <- daily_attendance_scope()
    students <- daily_attendance_students()
    if (!nzchar(scope$dept) || !nzchar(scope$semester) || nrow(students) == 0) {
      return(get_users()[0, , drop = FALSE])
    }
    users <- get_users()
    rows <- users[
      users$Role == "student" &
        users$RegNo %in% students$RegNo &
        users$CRAccess == "Yes" &
        users$CRDept == scope$dept &
        users$CRSemester == scope$semester,
      , drop = FALSE
    ]
    if (nzchar(scope$year)) rows <- rows[rows$CRYear == scope$year, , drop = FALSE]
    rows
  })

  output$daily_access_note <- renderUI({
    access <- daily_attendance_access()
    if (!access$can_mark) {
      return(tags$p("Attendance access is available only for faculty, HoD, principal and approved CR accounts.", style = "color:#64748b; font-weight:700;"))
    }
    if (access$is_cr) {
      assigned_by <- if (nrow(access$account) > 0 && nzchar(access$account$CRAssignedBy[1])) {
        paste("Assigned by:", access$account$CRAssignedBy[1], "|", access$account$CRAssignedByRole[1])
      } else {
        "CR access is active for your class."
      }
      return(tagList(
        span(class = "info-chip", paste("CR Class:", access$assigned_dept, "|", access$assigned_semester)),
        if (nzchar(access$assigned_year)) span(class = "info-chip", paste("Academic Year:", access$assigned_year)),
        span(class = "info-chip", assigned_by)
      ))
    }
    tagList(
      span(class = "info-chip", paste("Logged in as:", access$marker_role)),
      span(class = "info-chip", "You can assign or revoke CR access for the selected class."),
      span(class = "info-chip", "Select subject and class time before saving attendance.")
    )
  })

  output$daily_semester_ui <- renderUI({
    access <- daily_attendance_access()
    if (access$is_cr) {
      choices <- if (nzchar(access$assigned_semester)) stats::setNames(access$assigned_semester, access$assigned_semester) else c("SELECT" = "")
      return(selectInput(
        "daily_semester", "Semester",
        choices = choices,
        selected = if (nzchar(access$assigned_semester)) access$assigned_semester else ""
      ))
    }
    dept_value <- if (!is.null(input$daily_dept)) input$daily_dept else ""
    semesters <- if (nzchar(dept_value)) get_semester_choices(dept_value) else SEMESTER_OPTIONS
    selected_value <- if (!is.null(input$daily_semester) && input$daily_semester %in% semesters) input$daily_semester else ""
    selectInput("daily_semester", "Semester", choices = c("SELECT" = "", semesters), selected = selected_value)
  })

  output$daily_subject_ui <- renderUI({
    subject_info <- daily_subjects()
    subject_choices <- if (nrow(subject_info) > 0) {
      stats::setNames(subject_info$paper_code, paste(subject_info$paper_code, subject_info$subject, sep = " - "))
    } else {
      character()
    }
    selected_value <- if (!is.null(input$daily_subject) && input$daily_subject %in% subject_info$paper_code) input$daily_subject else ""
    selectInput("daily_subject", "Subject", choices = c("SELECT" = "", subject_choices), selected = selected_value)
  })

  output$daily_cr_assignment_ui <- renderUI({
    access <- daily_attendance_access()
    if (!access$can_assign) return(NULL)
    scope <- daily_attendance_scope()
    students <- daily_attendance_students()
    if (!nzchar(scope$dept) || !nzchar(scope$semester) || nrow(students) == 0) {
      return(
        div(
          style = "margin:18px 0; padding:18px; border-radius:18px; background:#f8fafc; border:1px solid #e2e8f0;",
          tags$div("Class Representative Access", style = "font-weight:900; font-size:20px; color:#0f172a; margin-bottom:8px;"),
          tags$p("Select department and semester first, then choose the class representative for that class.", style = "color:#64748b; font-weight:700; margin:0;")
        )
      )
    }
    current_cr <- current_class_cr()
    current_cr_text <- if (nrow(current_cr) > 0) {
      paste(
        ifelse(nzchar(current_cr$Name[1]), current_cr$Name[1], current_cr$RegNo[1]),
        "|",
        current_cr$RegNo[1]
      )
    } else {
      "No CR assigned for this class."
    }
    assignment_meta <- if (nrow(current_cr) > 0 && nzchar(current_cr$CRAssignedBy[1])) {
      paste("Assigned by", current_cr$CRAssignedBy[1], "|", current_cr$CRAssignedByRole[1])
    } else {
      "Choose a student below to grant CR attendance access."
    }
    student_choices <- stats::setNames(students$RegNo, paste(students$RegNo, students$Name, sep = " | "))
    selected_value <- if (!is.null(input$daily_cr_reg) && input$daily_cr_reg %in% students$RegNo) {
      input$daily_cr_reg
    } else if (nrow(current_cr) > 0) {
      current_cr$RegNo[1]
    } else {
      ""
    }
    div(
      style = "margin:18px 0; padding:18px; border-radius:18px; background:#f8fafc; border:1px solid #e2e8f0;",
      tags$div("Class Representative Access", style = "font-weight:900; font-size:20px; color:#0f172a; margin-bottom:12px;"),
      fluidRow(
        column(5, selectInput("daily_cr_reg", "CR Student", choices = c("SELECT" = "", student_choices), selected = selected_value)),
        column(4, tags$div(style = "padding-top:28px; color:#475569; font-weight:700;", paste("Current CR:", current_cr_text))),
        column(3, tags$div(style = "padding-top:28px; color:#64748b; font-weight:700;", assignment_meta))
      ),
      div(
        style = "display:flex; gap:14px; flex-wrap:wrap;",
        actionButton("assign_cr", "Assign CR Access", class = "btn-uibs"),
        actionButton("revoke_cr", "Revoke CR Access", class = "btn-uibs btn-uibs-outline")
      )
    )
  })

  observeEvent(input$assign_cr, {
    if (!can_assign_cr_access(user_role(), current_staff_designation())) {
      showNotification("Only faculty, HoD and principal accounts can assign CR access.", type = "error")
      return()
    }
    scope <- daily_attendance_scope()
    students <- daily_attendance_students()
    if (!nzchar(scope$dept) || !nzchar(scope$semester) || nrow(students) == 0) {
      showNotification("Select a class before assigning CR access.", type = "error")
      return()
    }
    if (is.null(input$daily_cr_reg) || !nzchar(input$daily_cr_reg)) {
      showNotification("Choose a student to assign as CR.", type = "error")
      return()
    }
    target_student <- students[students$RegNo == input$daily_cr_reg, , drop = FALSE]
    if (nrow(target_student) == 0) {
      showNotification("Selected student is not part of the current class.", type = "error")
      return()
    }
    users <- get_users()
    target_idx <- which(users$RegNo == input$daily_cr_reg & users$Role == "student")
    if (length(target_idx) == 0) {
      showNotification("The selected student needs a registered login account before CR access can be assigned.", type = "error")
      return()
    }
    class_regs <- students$RegNo
    clear_idx <- which(users$Role == "student" & users$RegNo %in% class_regs)
    if (length(clear_idx) > 0) {
      users[clear_idx, "CRAccess"] <- "No"
      users[clear_idx, "CRDept"] <- ""
      users[clear_idx, "CRYear"] <- ""
      users[clear_idx, "CRSemester"] <- ""
      users[clear_idx, "CRAssignedBy"] <- ""
      users[clear_idx, "CRAssignedByRole"] <- ""
      users[clear_idx, "CRAssignedOn"] <- ""
    }
    assigner_role <- if (nzchar(current_staff_designation())) current_staff_designation() else "Faculty"
    users[target_idx[1], "Name"] <- target_student$Name[1]
    users[target_idx[1], "Dept"] <- target_student$Dept[1]
    users[target_idx[1], "Year"] <- target_student$Year[1]
    users[target_idx[1], "CRAccess"] <- "Yes"
    users[target_idx[1], "CRDept"] <- scope$dept
    users[target_idx[1], "CRYear"] <- target_student$Year[1]
    users[target_idx[1], "CRSemester"] <- scope$semester
    users[target_idx[1], "CRAssignedBy"] <- daily_attendance_access()$marker_name
    users[target_idx[1], "CRAssignedByRole"] <- assigner_role
    users[target_idx[1], "CRAssignedOn"] <- as.character(Sys.time())
    users[target_idx[1], "UpdatedAt"] <- as.character(Sys.time())
    if (!write_users_db(users)) return()
    showNotification("CR access assigned successfully.", type = "message")
  })

  observeEvent(input$revoke_cr, {
    if (!can_assign_cr_access(user_role(), current_staff_designation())) {
      showNotification("Only faculty, HoD and principal accounts can revoke CR access.", type = "error")
      return()
    }
    scope <- daily_attendance_scope()
    students <- daily_attendance_students()
    if (!nzchar(scope$dept) || !nzchar(scope$semester) || nrow(students) == 0) {
      showNotification("Select a class before revoking CR access.", type = "error")
      return()
    }
    users <- get_users()
    revoke_idx <- which(
      users$Role == "student" &
        users$RegNo %in% students$RegNo &
        users$CRAccess == "Yes" &
        users$CRDept == scope$dept &
        users$CRSemester == scope$semester
    )
    if (length(revoke_idx) == 0) {
      showNotification("No CR access is active for this class.", type = "warning")
      return()
    }
    users[revoke_idx, "CRAccess"] <- "No"
    users[revoke_idx, "CRDept"] <- ""
    users[revoke_idx, "CRYear"] <- ""
    users[revoke_idx, "CRSemester"] <- ""
    users[revoke_idx, "CRAssignedBy"] <- ""
    users[revoke_idx, "CRAssignedByRole"] <- ""
    users[revoke_idx, "CRAssignedOn"] <- ""
    users[revoke_idx, "UpdatedAt"] <- as.character(Sys.time())
    if (!write_users_db(users)) return()
    showNotification("CR access removed for the selected class.", type = "message")
  })

  output$daily_attendance_cards <- renderUI({
    access <- daily_attendance_access()
    if (!access$can_mark) {
      return(tags$p("Only faculty, HoD, principal and approved CR logins can mark attendance.", style = "color:#64748b; font-weight:700;"))
    }
    scope <- daily_attendance_scope()
    students <- daily_attendance_students()
    if (!nzchar(scope$dept) || !nzchar(scope$semester)) {
      return(tags$p("Select department and semester to load the class attendance sheet.", style = "color:#64748b; font-weight:700;"))
    }
    if (nrow(students) == 0) {
      return(tags$p("No students found for the selected class.", style = "color:#64748b; font-weight:700;"))
    }
    subject_info <- daily_subjects()
    if (is.null(input$daily_subject) || !nzchar(input$daily_subject)) {
      return(tags$p("Choose a subject to open the attendance sheet.", style = "color:#64748b; font-weight:700;"))
    }
    subject_row <- subject_info[subject_info$paper_code == input$daily_subject, , drop = FALSE]
    if (nrow(subject_row) == 0) {
      return(tags$p("Choose a valid subject to continue.", style = "color:#64748b; font-weight:700;"))
    }
    records <- get_daily_attendance()
    date_value <- ifelse(is.null(input$daily_date) || !nzchar(input$daily_date), as.character(Sys.Date()), trimws(input$daily_date))
    time_value <- ifelse(is.null(input$daily_class_time), "", trimws(input$daily_class_time))
    class_time_match <- if (nzchar(time_value)) records$ClassTime == time_value else rep(FALSE, nrow(records))

    tagList(lapply(seq_len(nrow(students)), function(i) {
      photo <- safe_photo_path(students$Photo[i])
      existing <- records[
        records$Date == date_value &
          records$SubjectCode == subject_row$paper_code[1] &
          records$Dept == scope$dept &
          records$Semester == scope$semester &
          records$StudentRegNo == students$RegNo[i] &
          class_time_match,
        , drop = FALSE
      ]
      selected_status <- if (nrow(existing) > 0) existing$Status[1] else ""
      div(
        style = "display:flex; gap:18px; align-items:center; padding:18px; margin-bottom:14px; background:#f8fafc; border-radius:18px; border:1px solid #e2e8f0;",
        if (nzchar(photo)) {
          img(src = photo, style = "width:86px; height:86px; object-fit:cover; border-radius:22px;")
        } else {
          div(substr(students$Name[i], 1, 1), class = "profile-fallback", style = "width:86px; height:86px;")
        },
        div(
          style = "flex:1;",
          tags$div(students$Name[i], style = "font-weight:900; font-size:20px; color:#0f172a;"),
          tags$div(paste(students$RegNo[i], "|", students$Dept[i], "|", students$Year[i], "|", students$Semester[i]), style = "color:#64748b; font-weight:700;"),
          tags$div(full_address(students$AddressLine1[i], students$AddressLine2[i], students$City[i], students$State[i], students$Pincode[i]), style = "color:#475569;")
        ),
        selectInput(
          inputId = paste0("daily_status_", students$RegNo[i]),
          label = "Status",
          choices = c("SELECT" = "", "Present", "Absent"),
          selected = selected_status,
          width = "180px"
        )
      )
    }))
  })

  observeEvent(input$save_daily_attendance, {
    access <- daily_attendance_access()
    if (!access$can_mark) {
      showNotification("Only faculty, HoD, principal and approved CR logins can save attendance.", type = "error")
      return()
    }
    scope <- daily_attendance_scope()
    students <- daily_attendance_students()
    if (!nzchar(scope$dept) || !nzchar(scope$semester)) {
      showNotification("Select department and semester before saving attendance.", type = "error")
      return()
    }
    if (nrow(students) == 0) {
      showNotification("Load students before saving attendance.", type = "error")
      return()
    }
    subject_info <- daily_subjects()
    if (is.null(input$daily_subject) || !nzchar(input$daily_subject)) {
      showNotification("Select the subject before saving attendance.", type = "error")
      return()
    }
    subject_row <- subject_info[subject_info$paper_code == input$daily_subject, , drop = FALSE]
    if (nrow(subject_row) == 0) {
      showNotification("Select a valid subject before saving attendance.", type = "error")
      return()
    }
    time_value <- ifelse(is.null(input$daily_class_time), "", trimws(input$daily_class_time))
    if (!nzchar(time_value)) {
      showNotification("Enter the class time before saving attendance.", type = "error")
      return()
    }
    missing_status <- students$Name[vapply(seq_len(nrow(students)), function(i) {
      status_value <- input[[paste0("daily_status_", students$RegNo[i])]]
      is.null(status_value) || !nzchar(status_value)
    }, logical(1))]
    if (length(missing_status) > 0) {
      showNotification("Select Present or Absent for every student before saving.", type = "error")
      return()
    }
    records <- get_daily_attendance()
    date_value <- ifelse(is.null(input$daily_date) || !nzchar(input$daily_date), as.character(Sys.Date()), trimws(input$daily_date))
    records <- records[
      !(
        records$Date == date_value &
          records$Dept == scope$dept &
          records$Semester == scope$semester &
          records$SubjectCode == subject_row$paper_code[1] &
          records$ClassTime == time_value &
          records$StudentRegNo %in% students$RegNo
      ),
      , drop = FALSE
    ]

    new_rows <- lapply(seq_len(nrow(students)), function(i) {
      data.frame(
        Date = date_value,
        SubjectCode = subject_row$paper_code[1],
        SubjectName = subject_row$subject[1],
        ClassTime = time_value,
        MarkedByRegNo = user_reg(),
        MarkedByName = access$marker_name,
        MarkedByRole = access$marker_role,
        FacultyRegNo = user_reg(),
        FacultyName = access$marker_name,
        StudentRegNo = students$RegNo[i],
        StudentName = students$Name[i],
        Dept = students$Dept[i],
        Year = students$Year[i],
        Semester = students$Semester[i],
        Status = input[[paste0("daily_status_", students$RegNo[i])]],
        RecordedAt = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )
    })
    records <- rbind(records, do.call(rbind, new_rows))
    if (!write_daily_attendance_db(records)) return()
    showNotification("Subject attendance saved successfully.", type = "message")
  })

  selected_timetable_rows <- reactive({
    if (is.null(input$tt_dept) || !nzchar(input$tt_dept) || is.null(input$tt_semester) || !nzchar(input$tt_semester)) {
      return(timetable_template[0, , drop = FALSE])
    }
    rows <- get_timetable()
    rows <- rows[rows$Dept == input$tt_dept & rows$Semester == input$tt_semester, , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    rows$SlotOrder <- suppressWarnings(as.numeric(rows$SlotOrder))
    rows$SlotOrder[is.na(rows$SlotOrder)] <- seq_len(nrow(rows))
    rows$Day <- factor(rows$Day, levels = TIMETABLE_DAY_OPTIONS)
    rows <- rows[order(rows$SlotOrder, rows$Day, rows$Time), , drop = FALSE]
    rows$Day <- as.character(rows$Day)
    rows
  })

  output$timetable_editor_ui <- renderUI({
    can_edit <- can_manage_timetable(user_role(), current_staff_designation())
    if (!can_edit) {
      return(
        tags$div(
          class = "manager-form-section",
          div(class = "manager-form-title", "Published Time Table"),
          tags$p(
            "Only HoD, Principal and Admin can fill the blank timetable format. Students and staff will see the saved timetable here.",
            style = "color:#64748b; font-weight:700; margin-bottom:0;"
          )
        )
      )
    }

    if (is.null(input$tt_dept) || !nzchar(input$tt_dept) || is.null(input$tt_semester) || !nzchar(input$tt_semester)) {
      return(
        tags$div(
          class = "manager-form-section",
          div(class = "manager-form-title", "Blank Time Table Entry Form"),
          tags$p(
            "Select department and semester first. Then fill the blank format with day, time, subject and teacher name.",
            style = "color:#64748b; font-weight:700; margin-bottom:0;"
          )
        )
      )
    }

    current_rows <- selected_timetable_rows()
    slot_count <- timetable_slot_count(input$tt_dept, input$tt_semester, current_rows)
    subject_values <- unique(c(current_rows$Subject, get_subject_info(input$tt_dept, input$tt_semester)$subject))
    subject_values <- subject_values[nzchar(subject_values)]
    subject_choices <- c("SELECT" = "", stats::setNames(subject_values, subject_values))

    tagList(
      tags$div(
        class = "manager-form-section",
        div(class = "manager-form-title", "Blank Time Table Entry Form"),
        tags$p(
          "Fill each row properly. Leave unused rows fully blank. Saved rows will appear below for the selected department and semester.",
          style = "color:#64748b; font-weight:700;"
        ),
        lapply(seq_len(slot_count), function(i) {
          existing_row <- if (i <= nrow(current_rows)) current_rows[i, , drop = FALSE] else NULL
          fluidRow(
            column(
              3,
              selectInput(
                inputId = paste0("tt_day_", i),
                label = paste("Day", i),
                choices = c("SELECT" = "", TIMETABLE_DAY_OPTIONS),
                selected = if (!is.null(existing_row)) existing_row$Day[1] else ""
              )
            ),
            column(
              3,
              textInput(
                inputId = paste0("tt_time_", i),
                label = paste("Time", i),
                value = if (!is.null(existing_row)) existing_row$Time[1] else "",
                placeholder = "09:00 - 10:00"
              )
            ),
            column(
              3,
              selectInput(
                inputId = paste0("tt_subject_", i),
                label = paste("Subject", i),
                choices = subject_choices,
                selected = if (!is.null(existing_row) && existing_row$Subject[1] %in% names(subject_choices)) existing_row$Subject[1] else if (!is.null(existing_row)) existing_row$Subject[1] else ""
              )
            ),
            column(
              3,
              textInput(
                inputId = paste0("tt_teacher_", i),
                label = "Teacher Name",
                value = if (!is.null(existing_row)) existing_row$TeacherName[1] else ""
              )
            )
          )
        }),
        div(
          class = "manager-form-actions",
          actionButton("save_timetable", "Save Time Table", class = "btn-uibs")
        )
      )
    )
  })

  observeEvent(input$save_timetable, {
    if (!can_manage_timetable(user_role(), current_staff_designation())) {
      showNotification("Only HoD, Principal and Admin can save the timetable.", type = "error")
      return()
    }
    if (is.null(input$tt_dept) || !nzchar(input$tt_dept) || is.null(input$tt_semester) || !nzchar(input$tt_semester)) {
      showNotification("Select department and semester before saving the timetable.", type = "error")
      return()
    }

    current_rows <- selected_timetable_rows()
    slot_count <- timetable_slot_count(input$tt_dept, input$tt_semester, current_rows)
    partial_rows <- integer()
    new_rows <- list()

    for (i in seq_len(slot_count)) {
      day_value <- input[[paste0("tt_day_", i)]]
      time_value <- input[[paste0("tt_time_", i)]]
      subject_value <- input[[paste0("tt_subject_", i)]]
      teacher_value <- input[[paste0("tt_teacher_", i)]]

      day_value <- if (is.null(day_value)) "" else trimws(as.character(day_value))
      time_value <- if (is.null(time_value)) "" else trimws(as.character(time_value))
      subject_value <- if (is.null(subject_value)) "" else trimws(as.character(subject_value))
      teacher_value <- if (is.null(teacher_value)) "" else trimws(as.character(teacher_value))

      filled_flags <- c(nzchar(day_value), nzchar(time_value), nzchar(subject_value), nzchar(teacher_value))
      if (any(filled_flags) && !all(filled_flags)) {
        partial_rows <- c(partial_rows, i)
      } else if (all(filled_flags)) {
        new_rows[[length(new_rows) + 1]] <- data.frame(
          Dept = input$tt_dept,
          Semester = input$tt_semester,
          SlotOrder = i,
          Day = day_value,
          Time = time_value,
          Subject = subject_value,
          TeacherName = teacher_value,
          UpdatedByRegNo = ifelse(is.null(user_reg()), "", user_reg()),
          UpdatedByRole = ifelse(is.null(current_staff_designation()) || !nzchar(current_staff_designation()), user_role(), current_staff_designation()),
          UpdatedAt = as.character(Sys.time()),
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(partial_rows) > 0) {
      showNotification(
        paste("Complete all fields or leave the row blank. Check row(s):", paste(partial_rows, collapse = ", ")),
        type = "error",
        duration = 8
      )
      return()
    }

    timetable <- get_timetable()
    timetable <- timetable[!(timetable$Dept == input$tt_dept & timetable$Semester == input$tt_semester), , drop = FALSE]
    if (length(new_rows) > 0) {
      timetable <- rbind(timetable, do.call(rbind, new_rows))
    }
    if (!write_timetable_db(timetable)) return()

    if (length(new_rows) == 0) {
      showNotification("Time table cleared for the selected department and semester.", type = "message")
    } else {
      showNotification("Time table saved successfully.", type = "message")
    }
  })

  output$timetable_dt <- renderDT({
    if (is.null(input$tt_dept) || !nzchar(input$tt_dept) || is.null(input$tt_semester) || !nzchar(input$tt_semester)) {
      return(datatable(data.frame(Message = "Select department and semester to view the timetable.", stringsAsFactors = FALSE), options = list(dom = "t"), rownames = FALSE))
    }
    rows <- selected_timetable_rows()
    if (nrow(rows) == 0) {
      return(
        datatable(
          data.frame(Message = "No timetable saved yet. HoD can fill the blank timetable form for this semester.", stringsAsFactors = FALSE),
          options = list(dom = "t"),
          rownames = FALSE,
          selection = "none"
        )
      )
    }
    datatable(
      rows[, c("Day", "Time", "Subject", "TeacherName"), drop = FALSE],
      options = list(dom = "t", pageLength = 10),
      rownames = FALSE,
      selection = "none"
    )
  })

  output$college_highlights_ui <- renderUI({
    items <- college_highlights()
    tagList(lapply(seq_len(nrow(items)), function(i) {
      image_path <- if (nzchar(items$Image[i])) items$Image[i] else ""
      div(
        style = "display:flex; gap:18px; align-items:center; padding:20px; margin-bottom:18px; background:#fff; border-radius:18px; box-shadow:0 10px 25px rgba(15,23,42,0.08);",
        if (nzchar(items$Image[i])) {
          tags$img(src = items$Image[i], style = "width:180px; height:110px; object-fit:contain; border-radius:18px; background:#f8fafc; padding:10px;")
        },
        div(
          tags$div(items$Title[i], style = "font-weight:900; font-size:24px; color:#5b1f41;"),
          tags$p(items$Caption[i], style = "font-size:16px; color:#475569; margin-top:8px;")
        )
      )
    }))
  })

  output$calc_box <- renderUI({
    preview <- calc_preview()
    subject_info <- selected_subject_info()
    if (is.null(preview) || is.null(subject_info) || nrow(subject_info) == 0) {
      return(
        div(
          style = "color:#64748b; font-weight:700;",
          "No preview yet. Choose department and semester to start grade entry."
        )
      )
    }
    pass_marks <- get_pass_marks(subject_info)
    tagList(
      span(class = "info-chip", paste("Semester:", input$res_semester)),
      span(class = "info-chip", paste("Scheme:", unique(subject_info$scheme)[1])),
      span(class = "info-chip", paste("Credits:", get_total_credits(subject_info))),
      span(class = "info-chip", paste("Aggregate:", preview$total, "/", get_total_max(subject_info))),
      span(class = "info-chip", paste("Percentage:", format_percent(preview$percentage))),
      span(class = "info-chip", paste("SGPA:", round(preview$sgpa, 2))),
      span(class = "info-chip", paste("CGPA:", round(preview$cgpa, 2))),
      tags$h3(
        style = paste("font-weight:900; color:", grade_color(preview$grade), ";"),
        paste("Letter Grade:", preview$grade)
      ),
      if (any(preview$fail_flags)) {
        p(
          style = "color:#b91c1c; font-weight:700;",
          paste("Reappear required in:", paste(subject_info$subject[preview$fail_flags], collapse = ", "))
        )
      } else {
        p(style = "color:#0f766e; font-weight:700;", "All subjects satisfy minimum passing criteria.")
      },
      p(style = "color:#64748b; font-weight:700;", paste("Minimum pass marks:", paste(paste(subject_info$paper_code, pass_marks, sep = " "), collapse = " | ")))
    )
  })

  output$ops_attendance_ui <- renderUI({
    stu <- ops_student()
    if (is.null(stu)) {
      return(tags$p("Select a student to enter subject-wise attendance.", style = "color:#64748b; font-weight:700;"))
    }

    subject_info <- get_subject_info(stu$Dept[1], ifelse(!is.null(input$ops_semester) && nzchar(input$ops_semester), input$ops_semester, resolve_student_semester(stu)))
    if (nrow(subject_info) == 0) return(NULL)

    current_taken <- vapply(seq_len(nrow(subject_info)), function(i) {
      input_id <- paste0("ops_taken_", subject_info$code[i])
      value <- input[[input_id]]
      if (is.null(value)) stu[[attendance_taken_col(i)]][1] else as.numeric(value)
    }, numeric(1))

    current_attended <- vapply(seq_len(nrow(subject_info)), function(i) {
      input_id <- paste0("ops_attended_", subject_info$code[i])
      value <- input[[input_id]]
      base_value <- if (is.null(value)) stu[[attendance_attended_col(i)]][1] else as.numeric(value)
      min(base_value, current_taken[i])
    }, numeric(1))

    current_overall <- attendance_percentage_from_vectors(current_taken, current_attended)

    tagList(
      tags$p(
        paste("Overall Attendance (auto-calculated):", format_percent(current_overall)),
        style = "font-weight:800; color:#5b1f41;"
      ),
      tags$p(
        "Enter class-wise attendance for each subject. The system will calculate the overall attendance automatically.",
        style = "color:#64748b; font-weight:700;"
      ),
      tags$div(
        class = "attendance-grid",
        lapply(seq_len(nrow(subject_info)), function(i) {
          tags$div(
            class = "attendance-row",
            tags$div(
              class = "attendance-subject",
              paste(subject_info$paper_code[i], "-", subject_info$subject[i])
            ),
            numericInput(
              inputId = paste0("ops_taken_", subject_info$code[i]),
              label = "Classes Taken",
              value = current_taken[i],
              min = 0,
              step = 1
            ),
            numericInput(
              inputId = paste0("ops_attended_", subject_info$code[i]),
              label = "Classes Attended",
              value = current_attended[i],
              min = 0,
              step = 1
            ),
            tags$div(
              class = "attendance-percent",
              paste0(ifelse(current_taken[i] > 0, round_whole((current_attended[i] / current_taken[i]) * 100), 0), "%")
            )
          )
        })
      )
    )
  })

  observeEvent(input$save_btn, {
    if (!nzchar(input$res_id) || !nzchar(input$res_name) || !nzchar(input$res_dept) || !nzchar(input$res_year) || !nzchar(input$res_semester)) {
      showNotification("Load a student and complete all academic fields before publishing.", type = "error")
      return()
    }
    if (requires_language(input$res_dept, input$res_semester) && !nzchar(input$res_lang)) {
      showNotification("Select the language paper before publishing.", type = "error")
      return()
    }

    students <- ensure_schema(read_safe_csv(STUDENTS_DB, students_template), students_template)
    base_student <- latest_student_record(students, input$res_id)

    if (nrow(base_student) == 0) {
      showNotification("Student ID missing from registry.", type = "error")
      return()
    }

    preview <- calc_preview()
    subject_info <- selected_subject_info()
    idx <- which(students$RegNo == input$res_id & students$Semester == input$res_semester)

    if (length(idx) == 0) {
      new_student <- base_student[1, , drop = FALSE]
      new_student[paste0("M", 1:MAX_SUBJECT_SLOTS)] <- 0
      for (i in seq_len(MAX_SUBJECT_SLOTS)) {
        new_student[[attendance_taken_col(i)]] <- 0
        new_student[[attendance_attended_col(i)]] <- 0
      }
      new_student$Semester <- input$res_semester
      new_student$Scheme <- unique(subject_info$scheme)[1]
      new_student$Grade <- "N/A"
      new_student$Total <- 0
      new_student$Percentage <- 0
      new_student$SGPA <- 0
      new_student$CGPA <- 0
      new_student$Attendance <- 0
      new_student$UpdatedAt <- as.character(Sys.time())
      students <- rbind(students, new_student)
      idx <- nrow(students)
    } else {
      idx <- idx[1]
    }

    students[idx, "Name"] <- input$res_name
    students[idx, "Dept"] <- input$res_dept
    students[idx, "Year"] <- input$res_year
    students[idx, "Semester"] <- input$res_semester
    students[idx, "Scheme"] <- unique(subject_info$scheme)[1]
    students[idx, "Lang1"] <- if (requires_language(input$res_dept, input$res_semester)) input$res_lang else ""
    students[idx, paste0("M", 1:MAX_SUBJECT_SLOTS)] <- 0
    students[idx, subject_info$code] <- sanitize_marks(sapply(seq_len(nrow(subject_info)), function(i) input[[paste0("grade_", subject_info$code[i])]]), data.frame(max_mark = external_max_mark(subject_info$max_mark)))
    students[idx, "Total"] <- preview$total
    students[idx, "TotalCredits"] <- get_total_credits(subject_info)
    students[idx, "Percentage"] <- preview$percentage
    students[idx, "SGPA"] <- preview$sgpa
    students[idx, "PrevCGPA"] <- input$grade_prev
    students[idx, "CGPA"] <- preview$cgpa
    students[idx, "Grade"] <- preview$grade
    students[idx, "Mentor"] <- input$grade_mentor
    students[idx, "UpdatedAt"] <- as.character(Sys.time())

    if (!write_students_db(students)) return()
    showNotification("Official grade published successfully.", type = "message")
  })

  observeEvent(list(input$ops_reg, input$ops_semester), {
    req(input$ops_reg)
    db <- get_students()
    semester <- if (!is.null(input$ops_semester) && nzchar(input$ops_semester)) input$ops_semester else ""
    stu <- if (nzchar(semester)) {
      db[db$RegNo == input$ops_reg & db$Semester == semester, , drop = FALSE]
    } else {
      latest_student_record(db, input$ops_reg)
    }
    if (nrow(stu) == 0) {
      stu <- latest_student_record(db, input$ops_reg)
    }
    if (nrow(stu) == 0) return()
    updateTextInput(session, "ops_mentor", value = stu$Mentor[1])
  }, ignoreInit = TRUE)

  observeEvent(input$save_ops, {
    if (!nzchar(input$ops_reg) || is.null(input$ops_semester) || !nzchar(input$ops_semester)) {
      showNotification("Select a student and semester before updating operations.", type = "error")
      return()
    }

    students <- ensure_schema(read_safe_csv(STUDENTS_DB, students_template), students_template)
    idx <- which(students$RegNo == input$ops_reg & students$Semester == input$ops_semester)
    if (length(idx) == 0) {
      base_student <- latest_student_record(students, input$ops_reg)
      if (nrow(base_student) == 0) {
        showNotification("Student not found.", type = "error")
        return()
      }
      students <- rbind(students, base_student[1, , drop = FALSE])
      idx <- nrow(students)
      students[idx, "Semester"] <- input$ops_semester
      students[idx, "Scheme"] <- unique(get_subject_info(base_student$Dept[1], input$ops_semester)$scheme)[1]
    } else {
      idx <- idx[1]
    }

    stu <- students[idx, , drop = FALSE]
    subject_info <- get_subject_info(stu$Dept[1], input$ops_semester)
    for (i in seq_len(nrow(subject_info))) {
      taken_value <- suppressWarnings(as.numeric(input[[paste0("ops_taken_", subject_info$code[i])]]))
      attended_value <- suppressWarnings(as.numeric(input[[paste0("ops_attended_", subject_info$code[i])]]))
      taken_value <- ifelse(is.na(taken_value) || taken_value < 0, 0, round(taken_value))
      attended_value <- ifelse(is.na(attended_value) || attended_value < 0, 0, round(attended_value))
      attended_value <- min(attended_value, taken_value)
      students[idx, attendance_taken_col(i)] <- taken_value
      students[idx, attendance_attended_col(i)] <- attended_value
    }
    students[idx, "Attendance"] <- student_overall_attendance(students[idx, , drop = FALSE], subject_info)
    students[idx, "Mentor"] <- input$ops_mentor
    students[idx, "UpdatedAt"] <- as.character(Sys.time())
    if (!write_students_db(students)) return()

    showNotification("Operations record updated.", type = "message")
  })

  observeEvent(fee_student(), {
    stu <- fee_student()
    if (is.null(stu) || nrow(stu) == 0) return()
    updateTextInput(session, "fee_name", value = stu$Name[1])
    updateTextInput(session, "fee_dept", value = stu$Dept[1])
    updateSelectInput(session, "fee_status", selected = ifelse(nzchar(stu$FeeStatus[1]), stu$FeeStatus[1], "Pending"))
    updateNumericInput(session, "fee_total_amount", value = stu$FeeTotalAmount[1])
    updateNumericInput(session, "fee_paid_amount", value = stu$FeePaidAmount[1])
    updateNumericInput(session, "fee_scholarship_amount", value = stu$FeeScholarshipAmount[1])
    updateTextInput(session, "fee_last_payment_date", value = stu$FeeLastPaymentDate[1])
    updateTextInput(session, "fee_receipt_no", value = stu$FeeReceiptNo[1])
    updateTextAreaInput(session, "fee_remarks", value = stu$FeeRemarks[1])
  })

  output$fee_summary_ui <- renderUI({
    total_amount <- suppressWarnings(as.numeric(input$fee_total_amount))
    paid_amount <- suppressWarnings(as.numeric(input$fee_paid_amount))
    scholarship_amount <- suppressWarnings(as.numeric(input$fee_scholarship_amount))
    balance_amount <- calculate_fee_balance(total_amount, paid_amount, scholarship_amount)
    tagList(
      span(class = "info-chip", paste("Total Fee:", round_whole(ifelse(is.na(total_amount), 0, total_amount)))),
      span(class = "info-chip", paste("Paid:", round_whole(ifelse(is.na(paid_amount), 0, paid_amount)))),
      span(class = "info-chip", paste("Scholarship:", round_whole(ifelse(is.na(scholarship_amount), 0, scholarship_amount)))),
      span(class = "info-chip", paste("Balance:", balance_amount))
    )
  })

  output$fee_profile_ui <- renderUI({
    stu <- fee_student()
    if (is.null(stu) || nrow(stu) == 0) {
      return(tags$p("Select a student and semester to manage fee details.", style = "color:#64748b; font-weight:700;"))
    }
    photo <- safe_photo_path(stu$Photo[1])
    balance_amount <- calculate_fee_balance(stu$FeeTotalAmount[1], stu$FeePaidAmount[1], stu$FeeScholarshipAmount[1])
    tagList(
      div(
        class = "profile-shell",
        if (nzchar(photo)) {
          img(src = photo, class = "profile-img")
        } else {
          span(class = "profile-fallback", substr(stu$Name[1], 1, 1))
        },
        h3(stu$Name[1], style = "font-weight:900; margin-top:14px;"),
        p(paste(stu$RegNo[1], "|", stu$Dept[1], "|", stu$Semester[1]), style = "color:#64748b; font-weight:700;"),
        div(class = "stat-grid",
            div(class = "stat-card", span(class = "stat-val", stu$FeeStatus[1]), span(class = "stat-lbl", "Status")),
            div(class = "stat-card", span(class = "stat-val", round_whole(stu$FeePaidAmount[1])), span(class = "stat-lbl", "Paid")),
            div(class = "stat-card", span(class = "stat-val", round_whole(stu$FeeScholarshipAmount[1])), span(class = "stat-lbl", "Scholarship")),
            div(class = "stat-card", span(class = "stat-val", round_whole(balance_amount)), span(class = "stat-lbl", "Balance"))
        )
      )
    )
  })

  observeEvent(input$save_fee, {
    if (!nzchar(input$fee_reg) || is.null(input$fee_semester) || !nzchar(input$fee_semester)) {
      showNotification("Select a student and semester before updating fees.", type = "error")
      return()
    }

    students <- ensure_schema(read_safe_csv(STUDENTS_DB, students_template), students_template)
    idx <- which(students$RegNo == input$fee_reg & students$Semester == input$fee_semester)
    if (length(idx) == 0) {
      base_student <- latest_student_record(students, input$fee_reg)
      if (nrow(base_student) == 0) {
        showNotification("Student not found.", type = "error")
        return()
      }
      students <- rbind(students, base_student[1, , drop = FALSE])
      idx <- nrow(students)
      students[idx, "Semester"] <- input$fee_semester
      students[idx, "Scheme"] <- unique(get_subject_info(base_student$Dept[1], input$fee_semester)$scheme)[1]
    } else {
      idx <- idx[1]
    }

    balance_amount <- calculate_fee_balance(input$fee_total_amount, input$fee_paid_amount, input$fee_scholarship_amount)
    students[idx, "FeeStatus"] <- input$fee_status
    students[idx, "FeeTotalAmount"] <- ifelse(is.null(input$fee_total_amount), 0, input$fee_total_amount)
    students[idx, "FeePaidAmount"] <- ifelse(is.null(input$fee_paid_amount), 0, input$fee_paid_amount)
    students[idx, "FeeScholarshipAmount"] <- ifelse(is.null(input$fee_scholarship_amount), 0, input$fee_scholarship_amount)
    students[idx, "FeeBalanceAmount"] <- balance_amount
    students[idx, "FeeLastPaymentDate"] <- input$fee_last_payment_date
    students[idx, "FeeReceiptNo"] <- input$fee_receipt_no
    students[idx, "FeeRemarks"] <- input$fee_remarks
    students[idx, "UpdatedAt"] <- as.character(Sys.time())
    if (!write_students_db(students)) return()

    showNotification("Fee record updated.", type = "message")
  })

  render_student_profile_card <- function(stu) {
    if (is.null(stu) || nrow(stu) == 0) {
      return(tags$p("Select a student to view the profile.", style = "color:#64748b; font-weight:700;"))
    }
    photo <- safe_photo_path(stu$Photo[1])
    semester_label <- ifelse(nzchar(stu$Semester[1]), stu$Semester[1], "Semester not set")
    attendance_value <- student_overall_attendance(stu)
    tagList(
      div(
        class = "profile-shell",
        if (nzchar(photo)) {
          img(src = photo, class = "profile-img")
        } else {
          span(class = "profile-fallback", substr(stu$Name[1], 1, 1))
        },
        h3(stu$Name[1], style = "font-weight:900; margin-top:14px;"),
        p(paste(stu$RegNo[1], "|", stu$Dept[1], "|", stu$Year[1]), style = "color:#64748b; font-weight:700;"),
        p(paste(semester_label, "| Fee:", stu$FeeStatus[1]), style = "color:#64748b; font-weight:700;"),
        div(class = "stat-grid",
            div(class = "stat-card", span(class = "stat-val", stu$Grade[1]), span(class = "stat-lbl", "Grade")),
            div(class = "stat-card", span(class = "stat-val", round(stu$CGPA[1], 2)), span(class = "stat-lbl", "CGPA")),
            div(class = "stat-card", span(class = "stat-val", format_percent(stu$Percentage[1])), span(class = "stat-lbl", "Percentage")),
            div(class = "stat-card", span(class = "stat-val", format_percent(attendance_value)), span(class = "stat-lbl", "Attendance"))
        )
      )
    )
  }

  output$admin_student_profile <- renderUI({
    if (is.null(input$info_reg) || !nzchar(input$info_reg)) {
      return(render_student_profile_card(NULL))
    }
    render_student_profile_card(latest_student_record(get_students(), input$info_reg))
  })

  output$admin_dash_student_profile <- renderUI({
    if (is.null(input$dash_student) || !nzchar(input$dash_student)) {
      return(tags$p("Choose a student from the dashboard filter to view the photo and profile summary.", style = "color:#64748b; font-weight:700;"))
    }
    db <- get_students()
    semester <- if (!is.null(input$dash_semester) && nzchar(input$dash_semester)) input$dash_semester else ""
    stu <- if (nzchar(semester)) {
      rows <- db[db$RegNo == input$dash_student & db$Semester == semester, , drop = FALSE]
      if (nrow(rows) > 0) rows[1, , drop = FALSE] else latest_student_record(db, input$dash_student)
    } else {
      latest_student_record(db, input$dash_student)
    }
    render_student_profile_card(stu)
  })

  observeEvent(input$save_notice, {
    if (!nzchar(input$notice_title) || !nzchar(input$notice_body)) {
      showNotification("Announcement title and message are required.", type = "error")
      return()
    }

    notices <- ensure_schema(read_safe_csv(ANNOUNCEMENTS_DB, announcements_template), announcements_template)
    notices <- rbind(
      notices,
      data.frame(
        Title = input$notice_title,
        Body = input$notice_body,
        Audience = input$notice_audience,
        PostedOn = format(Sys.time(), "%Y-%m-%d %H:%M"),
        stringsAsFactors = FALSE
      )
    )
    if (!write_announcements_db(notices)) return()

    updateTextInput(session, "notice_title", value = "")
    updateTextAreaInput(session, "notice_body", value = "")
    updateSelectInput(session, "notice_audience", selected = "All")
    showNotification("Announcement published.", type = "message")
  })

  output$box_total <- renderValueBox({
    summary <- admin_summary()
    valueBox(summary$total_students, "Registered Students", icon = icon("users"), color = "purple")
  })

  output$box_avg <- renderValueBox({
    summary <- admin_summary()
    valueBox(format_percent(summary$avg_pct), "Average Percentage", icon = icon("chart-line"), color = "green")
  })

  output$box_pass <- renderValueBox({
    summary <- admin_summary()
    valueBox(summary$pass_count, "Students Passed", icon = icon("check-circle"), color = "teal")
  })

  output$box_fail <- renderValueBox({
    summary <- admin_summary()
    valueBox(summary$fail_count, "Students Requiring Reappear", icon = icon("times-circle"), color = "red")
  })

  output$g_dept <- renderPlot({
    db <- dashboard_students()
    counts <- table(factor(db$Dept, levels = DEPARTMENTS))
    barplot(counts, col = "#5b1f41", border = NA, las = 2)
  })

  output$g_year <- renderPlot({
    db <- dashboard_students()
    counts <- table(factor(db$Year, levels = YEARS))
    barplot(counts, col = "#ef6a3a", border = NA)
  })

  output$g_result <- renderPlot({
    db <- dashboard_students()
    active <- db[db$Grade != "N/A" & nzchar(db$Grade), , drop = FALSE]
    if (nrow(active) == 0) return(NULL)
    counts <- table(ifelse(active$Grade == "F", "Fail", "Pass"))
    colors <- c("Pass" = "#15803d", "Fail" = "#b91c1c")
    barplot(counts, col = colors[names(counts)], border = NA, main = "Result Distribution")
  })

  output$g_fail_subjects <- renderPlot({
    if (is.null(input$dash_semester) || !nzchar(input$dash_semester)) {
      plot.new()
      text(0.5, 0.5, "Select a semester to view subject-wise failures", cex = 1.1)
      return(invisible(NULL))
    }
    db <- dashboard_students()
    if (nrow(db) == 0) return(NULL)
    dept_for_chart <- if (nzchar(input$dash_dept)) input$dash_dept else if (length(unique(db$Dept)) == 1) unique(db$Dept)[1] else "BCA"
    subject_info <- get_subject_info(dept_for_chart, input$dash_semester)
    fail_counts <- sapply(seq_len(nrow(subject_info)), function(i) {
      col_name <- subject_info$code[i]
      threshold <- get_pass_marks(subject_info)[i]
      sum(db[[col_name]] < threshold, na.rm = TRUE)
    })
    names(fail_counts) <- paste(subject_info$paper_code, subject_info$subject, sep = " - ")
    par(mar = c(5, 14, 2, 2))
    barplot(fail_counts, horiz = TRUE, las = 1, col = "#dc2626", border = NA)
  })

  output$g_sub_avgs <- renderPlot({
    if (is.null(input$dash_semester) || !nzchar(input$dash_semester)) {
      plot.new()
      text(0.5, 0.5, "Select a semester to view subject performance", cex = 1.1)
      return(invisible(NULL))
    }
    db <- dashboard_students()
    if (nrow(db) == 0) return(NULL)
    dept_for_chart <- if (nzchar(input$dash_dept)) input$dash_dept else if (length(unique(db$Dept)) == 1) unique(db$Dept)[1] else "BCA"
    subject_info <- get_subject_info(dept_for_chart, input$dash_semester)
    avgs <- colMeans(db[, subject_info$code, drop = FALSE], na.rm = TRUE)
    pct_of_max <- (avgs / subject_info$max_mark) * 100
    par(mar = c(5, 14, 2, 2))
    barplot(pct_of_max, names.arg = paste(subject_info$paper_code, subject_info$subject, sep = " - "), horiz = TRUE, las = 1, col = "#7c2d12", border = NA, xlab = "Average Score % of Max")
  })

  output$g_attendance_band <- renderPlot({
    db <- with_attendance_metric(dashboard_students())
    bands <- cut(
      db$AttendanceMetric,
      breaks = c(-Inf, 59.99, 74.99, 84.99, Inf),
      labels = c("Below 60%", "60-74%", "75-84%", "85%+")
    )
    counts <- table(bands)
    barplot(counts, col = c("#b91c1c", "#f59e0b", "#0ea5e9", "#15803d"), border = NA, las = 2)
  })

  output$g_grade_interactive <- renderPlot({
    db <- dashboard_students()
    active <- db[db$Grade != "N/A" & nzchar(db$Grade), , drop = FALSE]
    if (nrow(active) == 0) {
      plot.new()
      text(0.5, 0.5, "No published grades yet", col = "#64748b", cex = 1.1)
      return(invisible(NULL))
    }

    grade_counts <- table(active$Grade)
    colors <- c("A+" = "#1f7a4d", "A" = "#0f766e", "B" = "#d97706", "C" = "#b45309", "F" = "#b91c1c")
    pie(
      grade_counts,
      labels = paste0(names(grade_counts), " (", grade_counts, ")"),
      col = colors[names(grade_counts)],
      border = "white",
      main = "Grade Composition"
    )
  })

  output$g_attendance_vs_result <- renderPlot({
    db <- with_attendance_metric(dashboard_students())
    active <- db[db$Grade != "N/A" & nzchar(db$Grade), , drop = FALSE]
    if (nrow(active) == 0) return(NULL)
    cols <- ifelse(active$Grade == "F", "#dc2626", "#15803d")
    plot(
      active$AttendanceMetric, active$Percentage,
      pch = 19, col = cols,
      xlab = "Attendance %",
      ylab = "Percentage",
      main = "Attendance and Result Quality"
    )
  })

  output$g_fee_status <- renderPlot({
    db <- dashboard_students()
    fee_table <- table(factor(db$FeeStatus, levels = FEE_OPTIONS))
    barplot(fee_table, col = c("#15803d", "#dc2626", "#2563eb"), border = NA)
  })

  output$master_dt <- renderDT({
    registry <- with_attendance_metric(filtered_registry())
    registry$Attendance <- registry$AttendanceMetric
    datatable(
      registry[, c("RegNo", "Name", "Dept", "Year", "Semester", "Scheme", "Grade", "Percentage", "Attendance", "FeeStatus", "Mentor", "UpdatedAt"), drop = FALSE],
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      selection = "none"
    )
  })

  output$topper_dt <- renderDT({
    db <- get_students()
    top_db <- db[order(-db$Percentage, db$Name), c("RegNo", "Name", "Dept", "Percentage", "CGPA"), drop = FALSE]
    datatable(head(top_db, 8), options = list(dom = "t", pageLength = 8), rownames = FALSE, selection = "none")
  })

  output$stu_profile <- renderUI({
    stu <- current_student()
    if (nrow(stu) == 0) return(h4("Student record unavailable."))
    snapshot <- student_snapshot(stu)

    photo <- safe_photo_path(stu$Photo[1])

    tagList(
      div(
        class = "profile-shell",
        if (nzchar(photo)) {
          img(src = photo, class = "profile-img")
        } else {
          span(class = "profile-fallback", substr(stu$Name[1], 1, 1))
        },
        h3(stu$Name[1], style = "font-weight:900; margin-top:14px;"),
        p(
          paste(stu$Dept[1], "|", stu$Year[1], "|", snapshot$semester, "|", snapshot$scheme),
          style = "color:#64748b; font-weight:700;"
        ),
        p(
          full_address(stu$AddressLine1[1], stu$AddressLine2[1], stu$City[1], stu$State[1], stu$Pincode[1]),
          style = "color:#475569; font-weight:700;"
        ),
        div(
          class = "stat-grid",
          div(class = "stat-card", span(class = "stat-val", snapshot$results$grade), span(class = "stat-lbl", "Grade")),
          div(class = "stat-card", span(class = "stat-val", round(snapshot$results$cgpa, 2)), span(class = "stat-lbl", "CGPA")),
          div(class = "stat-card", span(class = "stat-val", format_percent(snapshot$results$percentage)), span(class = "stat-lbl", "Percentage")),
          div(class = "stat-card", span(class = "stat-val", format_percent(student_overall_attendance(stu, snapshot$subject_info))), span(class = "stat-lbl", "Overall Attendance"))
        )
      )
    )
  })

  output$stu_benchmark <- renderPlot({
    db <- get_students()
    stu <- current_student()
    if (nrow(stu) == 0 || nrow(db) == 0) return(NULL)
    if (!is_results_published(stu)) {
      plot.new()
      text(0.5, 0.5, "Marks have not been published yet.", cex = 1.1)
      return(invisible(NULL))
    }
    snapshot <- student_snapshot(stu)
    subject_info <- snapshot$subject_info
    peer_db <- db[db$Dept == stu$Dept[1] & db$Semester == snapshot$semester, , drop = FALSE]
    if (nrow(peer_db) == 0) peer_db <- db
    college_avg <- colMeans(peer_db[, subject_info$code, drop = FALSE], na.rm = TRUE)
    my_scores <- snapshot$marks
    par(mar = c(8, 4, 3, 2))
    barplot(
      rbind(my_scores, college_avg),
      beside = TRUE,
      names.arg = paste(subject_info$paper_code, subject_info$subject, sep = " - "),
      las = 2,
      col = c("#ef6a3a", "#cbd5e1"),
      border = NA,
      main = "My Scores vs College Average"
    )
    legend("topright", legend = c("Me", "College Avg"), fill = c("#ef6a3a", "#cbd5e1"), bty = "n")
  })

  output$stu_progress_plot <- renderPlot({
    stu <- current_student()
    if (nrow(stu) == 0) return(NULL)
    if (!is_results_published(stu)) return(NULL)
    snapshot <- student_snapshot(stu)
    scores <- c(stu$PrevCGPA[1], snapshot$results$sgpa, snapshot$results$cgpa)
    names(scores) <- c("Previous CGPA", "Current SGPA", "Current CGPA")

    plot(
      seq_along(scores),
      scores,
      type = "b",
      pch = 19,
      lwd = 3,
      col = "#5b1f41",
      xaxt = "n",
      ylim = c(0, 10),
      xlab = "",
      ylab = "Score",
      main = "Academic Trend"
    )
    axis(1, at = seq_along(scores), labels = names(scores))
    grid(nx = NA, ny = NULL, col = "#e5e7eb")
  })

  output$student_service_cards <- renderUI({
    stu <- current_student()
    if (nrow(stu) == 0) return(NULL)
    snapshot <- student_snapshot(stu)

    overall_attendance <- student_overall_attendance(stu, snapshot$subject_info)
    attendance_ready <- is_attendance_eligible(stu, snapshot$subject_info)
    attendance_flag <- if (attendance_ready) "Eligible" else "Not Eligible"
    attendance_color <- if (attendance_ready) "#0f766e" else "#b91c1c"
    fee_color <- if (stu$FeeStatus[1] == "Paid") "#0f766e" else "#b45309"
    fee_balance <- calculate_fee_balance(stu$FeeTotalAmount[1], stu$FeePaidAmount[1], stu$FeeScholarshipAmount[1])

    tagList(
      div(class = "info-chip", paste("Mentor:", ifelse(nzchar(stu$Mentor[1]), stu$Mentor[1], "Not assigned"))),
      div(class = "info-chip", paste("Address:", full_address(stu$AddressLine1[1], stu$AddressLine2[1], stu$City[1], stu$State[1], stu$Pincode[1]))),
      div(class = "info-chip", paste("Fee Status:", stu$FeeStatus[1])),
      div(class = "info-chip", paste("Fee Paid:", round_whole(stu$FeePaidAmount[1]))),
      div(class = "info-chip", paste("Fee Balance:", round_whole(fee_balance))),
      if (requires_language(stu$Dept[1], snapshot$semester) && nzchar(stu$Lang1[1])) div(class = "info-chip", paste("Language 1:", stu$Lang1[1])),
      div(class = "info-chip", paste("Overall Attendance:", format_percent(overall_attendance))),
      tags$hr(),
      tags$p(style = paste("font-weight:800; color:", attendance_color, ";"), paste("Attendance Eligibility:", attendance_flag)),
      tags$p(style = paste("font-weight:800; color:", fee_color, ";"), paste("Finance Standing:", stu$FeeStatus[1])),
      tags$p(style = "font-weight:700; color:#475569;", paste("Last updated:", ifelse(nzchar(stu$UpdatedAt[1]), stu$UpdatedAt[1], "Not recorded")))
    )
  })

  output$stu_marks_dt <- renderDT({
    stu <- current_student()
    if (nrow(stu) == 0) return(datatable(data.frame()))
    if (!is_results_published(stu)) {
      return(datatable(
        data.frame(Message = "Marks have not been published yet.", stringsAsFactors = FALSE),
        options = list(dom = "t"),
        rownames = FALSE,
        selection = "none"
      ))
    }
    datatable(build_marksheet(stu), options = list(pageLength = 8, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE, selection = "none")
  })

  output$student_summary_panel <- renderUI({
    stu <- current_student()
    if (nrow(stu) == 0) return(NULL)
    if (!is_results_published(stu)) {
      return(tags$p("Performance summary will appear after staff publish the official marks.", style = "color:#64748b; font-weight:700;"))
    }
    snapshot <- student_snapshot(stu)
    subject_info <- snapshot$subject_info
    fee_balance <- calculate_fee_balance(stu$FeeTotalAmount[1], stu$FeePaidAmount[1], stu$FeeScholarshipAmount[1])

    tagList(
      h3(paste("Overall Grade:", snapshot$results$grade), style = paste("font-weight:900; color:", grade_color(snapshot$results$grade), ";")),
      p(paste("Semester:", snapshot$semester, "| Scheme:", snapshot$scheme), style = "font-weight:700;"),
      p(paste("Total Marks:", snapshot$results$total, "/", get_total_max(subject_info)), style = "font-weight:700;"),
      p(paste("Credits:", get_total_credits(subject_info))),
      p(paste("Percentage:", format_percent(snapshot$results$percentage))),
      p(paste("SGPA:", round(snapshot$results$sgpa, 2))),
      p(paste("CGPA:", round(snapshot$results$cgpa, 2))),
      p(paste("Overall Attendance:", format_percent(student_overall_attendance(stu, subject_info)))),
      p(paste("Fee Status:", stu$FeeStatus[1])),
      p(paste("Fee Paid:", round_whole(stu$FeePaidAmount[1]))),
      p(paste("Fee Balance:", round_whole(fee_balance)))
    )
  })

  output$student_services_status <- renderUI({
    stu <- current_student()
    if (nrow(stu) == 0) return(NULL)

    overall_attendance <- student_overall_attendance(stu)
    attendance_ready <- is_attendance_eligible(stu)
    fee_clear <- identical(stu$FeeStatus[1], "Paid") || identical(stu$FeeStatus[1], "Scholarship")
    fee_balance <- calculate_fee_balance(stu$FeeTotalAmount[1], stu$FeePaidAmount[1], stu$FeeScholarshipAmount[1])

    tagList(
      p(paste("Overall Attendance:", format_percent(overall_attendance)), style = "font-weight:800;"),
      p(paste("Fee Status:", stu$FeeStatus[1]), style = "font-weight:800;"),
      p(paste("Fee Paid:", round_whole(stu$FeePaidAmount[1])), style = "font-weight:800;"),
      p(paste("Fee Balance:", round_whole(fee_balance)), style = "font-weight:800;"),
      p(paste("Address:", full_address(stu$AddressLine1[1], stu$AddressLine2[1], stu$City[1], stu$State[1], stu$Pincode[1])), style = "font-weight:800;"),
      p(paste("Mentor:", ifelse(nzchar(stu$Mentor[1]), stu$Mentor[1], "Not assigned")), style = "font-weight:800;"),
      tags$hr(),
      p(
        if (attendance_ready) "Attendance requirement satisfied. Student is eligible."
        else "Attendance is below 75%. Student is not eligible.",
        style = paste("font-weight:700; color:", if (attendance_ready) "#0f766e" else "#b91c1c", ";")
      ),
      p(
        if (fee_clear) "Finance status supports normal document processing."
        else "Pending finance clearance may delay some administrative approvals.",
        style = paste("font-weight:700; color:", if (fee_clear) "#0f766e" else "#b45309", ";")
      )
    )
  })

  render_notice_feed <- function(notices) {
    if (nrow(notices) == 0) {
      return(tags$p("No announcements published yet.", style = "color:#64748b; font-weight:700;"))
    }

    tagList(lapply(seq_len(min(nrow(notices), 8)), function(i) {
      div(
        class = "notice-card",
        div(class = "notice-title", notices$Title[i]),
        div(class = "notice-meta", paste(notices$Audience[i], "|", notices$PostedOn[i])),
        div(notices$Body[i])
      )
    }))
  }

  output$notice_feed_admin <- renderUI({
    render_notice_feed(get_announcements()[order(get_announcements()$PostedOn, decreasing = TRUE), , drop = FALSE])
  })

  output$notice_feed_student <- renderUI({
    render_notice_feed(announcements_for_user())
  })

  output$dl_transcript <- downloadHandler(
    filename = function() paste0(user_reg(), "_Official_Transcript.pdf"),
    content = function(file) {
      stu <- current_student()
      if (nrow(stu) == 0) return(NULL)
      marksheet <- build_marksheet(stu)
      snapshot <- student_snapshot(stu)
      subject_info <- snapshot$subject_info

      pdf(file, width = 8.27, height = 11.69)
      plot.new()
      plot.window(xlim = c(0, 100), ylim = c(0, 100))

      rect(0, 0, 100, 100, border = "#5b1f41", lwd = 2)
      rect(0, 88, 100, 100, col = "#5b1f41", border = NA)
      text(50, 95, "UNITED INTERNATIONAL BUSINESS SCHOOL", col = "white", cex = 1.6, font = 2)
      text(50, 91.5, "BENGALURU CAMPUS | OFFICIAL ACADEMIC TRANSCRIPT", col = "white", cex = 0.95, font = 2)

      logo_raster <- read_image_raster(app_logo_file())
      if (!is.null(logo_raster)) rasterImage(logo_raster, 3, 89, 14, 99)

      rect(5, 73, 95, 86, border = "#e2e8f0", col = "#f8fafc")
      text(8, 83.5, paste("NAME:", toupper(stu$Name[1])), adj = 0, font = 2, cex = 1.1)
      text(8, 80.5, paste("REG ID:", stu$RegNo[1]), adj = 0, cex = 1.0)
      text(8, 77.5, paste("DEPARTMENT:", stu$Dept[1]), adj = 0, cex = 1.0)
      text(8, 74.5, paste("ACADEMIC YEAR:", stu$Year[1], "|", snapshot$semester, "|", snapshot$scheme), adj = 0, cex = 0.95)

      photo <- safe_photo_path(stu$Photo[1])
      if (nzchar(photo)) {
        photo_raster <- read_image_raster(file.path("www", photo))
        if (!is.null(photo_raster)) rasterImage(photo_raster, 82, 74, 93, 85)
      }

      rect(5, 66, 95, 70, col = "#5b1f41", border = NA)
      text(8, 68, "SUBJECT", col = "white", adj = 0, font = 2)
      text(70, 68, "MAX", col = "white", adj = 1, font = 2)
      text(92, 68, "SCORE", col = "white", adj = 1, font = 2)

      y <- 62
      for (i in seq_len(nrow(marksheet))) {
        text(8, y, marksheet$Subject[i], adj = 0, cex = 0.95)
        text(70, y, marksheet$Max[i], adj = 1, cex = 0.95)
        text(92, y, marksheet$Score[i], adj = 1, cex = 1.0, font = 2)
        segments(5, y - 1.5, 95, y - 1.5, col = "#e2e8f0")
        y <- y - 5
      }

      rect(5, 14, 95, 28, border = "#ef6a3a", lwd = 2, col = "#fff8f4")
      text(8, 24, paste("TOTAL:", snapshot$results$total, "/", get_total_max(subject_info)), adj = 0, font = 2)
      text(8, 20, paste("PERCENTAGE:", format_percent(snapshot$results$percentage)), adj = 0)
      text(42, 20, paste("SGPA:", round(snapshot$results$sgpa, 2)), adj = 0)
      text(62, 20, paste("CGPA:", round(snapshot$results$cgpa, 2)), adj = 0)
      text(92, 24, paste("GRADE:", snapshot$results$grade), adj = 1, font = 2, col = grade_color(snapshot$results$grade))
      text(8, 16, paste("MENTOR:", ifelse(nzchar(stu$Mentor[1]), stu$Mentor[1], "Not assigned")), adj = 0, cex = 0.9)
      if (requires_language(stu$Dept[1], snapshot$semester) && nzchar(stu$Lang1[1])) {
        text(92, 16, paste("LANGUAGE 1:", stu$Lang1[1]), adj = 1, cex = 0.9)
      }
      text(50, 5, "Digitally generated institutional record | UIBS Bengaluru", cex = 0.65, col = "#64748b")
      dev.off()
    }
  )

  output$dl_id <- downloadHandler(
    filename = function() paste0(user_reg(), "_UIBS_ID.pdf"),
    content = function(file) {
      stu <- current_student()
      if (nrow(stu) == 0) return(NULL)

      pdf(file, width = 3.375, height = 2.125, paper = "special", onefile = FALSE, family = "Helvetica", useDingbats = FALSE)
      op <- setup_card_plot()
      on.exit({
        par(op)
        dev.off()
      }, add = FALSE)

      rect(1, 1, 99, 99, border = "#5b1f41", lwd = 5)
      rect(1, 77, 99, 99, col = "#5b1f41", border = NA)
      rect(1, 1, 99, 15, col = "#ef6a3a", border = NA)
      text(50, 89.5, "UIBS BENGALURU", col = "white", cex = fit_text_cex("UIBS BENGALURU", 72, base_cex = 1.08, font = 2), font = 2)
      text(50, 81.5, "OFFICIAL STUDENT ID", col = "white", cex = fit_text_cex("OFFICIAL STUDENT ID", 54, base_cex = 0.46, min_cex = 0.36, font = 2), font = 2)
      text(50, 8, "IN PURSUIT OF EXCELLENCE", col = "white", cex = 0.7, font = 2)

      photo <- safe_photo_path(stu$Photo[1])
      if (nzchar(photo)) {
        photo_raster <- read_image_raster(file.path("www", photo))
        draw_image_cover(photo_raster, 5, 24, 36, 69)
      }

      student_name <- toupper(substr(stu$Name[1], 1, 32))
      text(40, 62, student_name, adj = 0, font = 2, cex = fit_text_cex(student_name, 54, base_cex = 0.88, min_cex = 0.52, font = 2))
      text(40, 51, paste("REG ID:", stu$RegNo[1]), adj = 0, cex = 0.75)
      text(40, 42, paste("DEPT:", stu$Dept[1]), adj = 0, cex = 0.75)
      text(40, 33, paste("YEAR:", stu$Year[1]), adj = 0, cex = 0.75)
      text(40, 24, paste("VALIDITY:", format(Sys.Date() + 365, "%b %Y")), adj = 0, cex = 0.55, col = "#64748b")
    }
  )
}
app <- shinyApp(ui = ui, server = server)
