// Date and time operations
const now = new Date();
const timestamp = now.getTime();
const year = now.getFullYear();
const month = now.getMonth() + 1; // Months are 0-indexed
const day = now.getDate();
const hours = now.getHours();
const minutes = now.getMinutes();
const seconds = now.getSeconds();

// Creating specific dates
const specificDate = new Date(2023, 0, 15); // January 15, 2023
const dateFromString = new Date("2023-05-20T14:30:00");
const dateFromTimestamp = new Date(1672531200000); // January 1, 2023

// Date calculations
const tomorrow = new Date();
tomorrow.setDate(tomorrow.getDate() + 1);

const nextMonth = new Date();
nextMonth.setMonth(nextMonth.getMonth() + 1);

// Formatting date as string
const dateString = now.toDateString();
const timeString = now.toTimeString();
const isoString = now.toISOString();
const localeString = now.toLocaleString();

console.log("Current timestamp:", timestamp);
console.log("Date components:", year, month, day);
console.log("Time components:", hours, minutes, seconds);
console.log("Specific date:", specificDate);
console.log("Date from string:", dateFromString);
console.log("Date from timestamp:", dateFromTimestamp);
console.log("Tomorrow:", tomorrow);
console.log("Next month:", nextMonth);
console.log("Date string:", dateString);
console.log("ISO string:", isoString);
console.log("Locale string:", localeString);
