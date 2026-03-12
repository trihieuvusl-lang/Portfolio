-- Cleaning Data in SQL Queries

Select * 
From Portfolio_Project.dbo.NashvilleHousing

-- Standarize Date Format

Select SaleDateConverted, CONVERT(date, SaleDate)
From Portfolio_Project.dbo.NashvilleHousing

Update NashvilleHousing
Set SaleDate = CONVERT(date, SaleDate)

ALTER TABLE NashvilleHousing
Add SaleDateConverted Date;

Update NashvilleHousing
SET SaleDateConverted = CONVERT(date, SaleDate)

-- Populate Property Address data

Select * 
From Portfolio_Project.dbo.NashvilleHousing
--Where PropertyAddress is null
Order by ParcelID

Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress,b.PropertyAddress)
From Portfolio_Project.dbo.NashvilleHousing a
JOIN Portfolio_Project.dbo.NashvilleHousing b
	on a.ParcelID = b.ParcelID
	and a.[UniqueID ] <> b.[UniqueID ]  
Where a.PropertyAddress is NULL

Update a
Set PropertyAddress = ISNULL(a.PropertyAddress,b.PropertyAddress)
From Portfolio_Project.dbo.NashvilleHousing a
JOIN Portfolio_Project.dbo.NashvilleHousing b
	on a.ParcelID = b.ParcelID
	and a.[UniqueID ] <> b.[UniqueID ]  


-- Breaking out Address into Individual Collumns (Address, City, State)
Select PropertyAddress
From Portfolio_Project.dbo.NashvilleHousing

SELECT 
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1 ) as Address
, SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1 , LEN(PropertyAddress)) as Address

From Portfolio_Project.dbo.NashvilleHousing

ALTER TABLE NashvilleHousing
Add PropertySplitAddress Nvarchar(255);

Update NashvilleHousing
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1 )

ALTER TABLE NashvilleHousing
Add PropertySplitCity Nvarchar(255);

Update NashvilleHousing
SET PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1 , LEN(PropertyAddress)) 

Select * From Portfolio_Project.dbo.NashvilleHousing


ALTER TABLE NashvilleHousing
DROP COLUMN PropertySplitAddress;


Select OwnerAddress
From Portfolio_Project.dbo.NashvilleHousing


Select
PARSENAME(Replace(OwnerAddress, ',', '.'),  3) -- Replace(OwnerAddress, ',', '.') means replace , -> . b/c PARSENAME only work with . 
, PARSENAME(Replace(OwnerAddress, ',', '.'), 2 ) -- PARSENAME ~ modified string and extracts the 1st,2nd,3rd part from the right.
, PARSENAME(Replace(OwnerAddress, ',', '.'), 1 )
From Portfolio_Project.dbo.NashvilleHousing


ALTER TABLE NashvilleHousing
Add OwnerSplitAddress Nvarchar(255);

Update NashvilleHousing
SET OwnerSplitAddress = PARSENAME(Replace(OwnerAddress, ',', '.'),  3)

ALTER TABLE NashvilleHousing
Add OwnerSplitCity Nvarchar(255);

Update NashvilleHousing
SET OwnerSplitCity = PARSENAME(Replace(OwnerAddress, ',', '.'), 2 )

ALTER TABLE NashvilleHousing
Add OwnerSplitState Nvarchar(255);

Update NashvilleHousing
SET OwnerSplitState = PARSENAME(Replace(OwnerAddress, ',', '.'), 1 )


Select *
From Portfolio_Project.dbo.NashvilleHousing



-- Change Y and N to Yes and No in 'Sold as Vacant' field

Select Distinct(SoldAsVacant) -- take current values from a column without repetition ~ unique value
, COUNT(SoldAsVacant)
From Portfolio_Project.dbo.NashvilleHousing
Group by SoldAsVacant
Order by 2


Select SoldAsVacant
, CASE When SoldAsVacant = 'Y' THEN 'YES'  -- similar to IF ELSE 
, CASE When SoldAsVacant = 'N' THEN 'No'
	ELSE SoldAsVacant
	END
From Portfolio_Project.dbo.NashvilleHousing

Update NashvilleHousing
SET SoldAsVacant = CASE When SoldAsVacant = 'Y' THEN 'YES'  
	When SoldAsVacant = 'N' THEN 'No'
	ELSE SoldAsVacant
	END


-- Remove Duplicates
-- CTE ~ temporary result/table set can reference within a SELECT, INSERT, UPDATE, or DELETE statement
WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() OVER (
	PARTITION BY ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY 
					UniqueID
					) row_num
From Portfolio_Project.dbo.NashvilleHousing
)
Select *
FROM RowNumCTE
Where row_num > 1

-- Delete Unused Columns

Select *
From Portfolio_Project.dbo.NashvilleHousing

ALTER TABLE Portfolio_Project.dbo.NashvilleHousing
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress

ALTER TABLE Portfolio_Project.dbo.NashvilleHousing
DROP COLUMN SaleDAte


